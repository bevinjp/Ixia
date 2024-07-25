
package require math::statistics
package require json::write

lappend auto_path [file normalize "../lib"]
package require TestLibrary
package require ChartLibrary
package require CloudshellLibrary
namespace import -force LoggerLibrary::write

set tput_up   0
set tput_down 0

# Used for aggrating verification at the very end
set verification_calls ""

##
#\addtogroup Tests
# @{

##
#\file IxOs_L2.tcl
#\author <a href=mailto:kevin.wren@viasat.com> Kevin Wren</a>
#\brief Generates Ixia traffic for L2 NBN networks.
#\attention Only works on NBN networks!
#
#<b> Default Script Parameters (recommend override in .prm file):</b>
#\verbatim
set ::test::smmac       "e0 01 03 00 00 01"; # starting MAC address
# user port MAC addresses can be specified using a single start value for all id ranges
# or in a list with a starting MAC address defined for each id range
#set ::test::smmac       {"e0 01 11 00 00 01" "e0 01 21 00 00 01" "e0 01 31 00 00 01"}

# destination mac can be specified in one of 3 forms:
# empty string, meaning use arp (default): ""
# single value: "00 a0 bc 01 23 45"
# start value/step pair, incremented for each user: {"00 a0 bc 01 23 45" 1}
set ::test::destMac_up       ""
set ::test::destMac_down     ""

set ::test::upstreamEnable   0
set ::test::downstreamEnable 0

# durationMode: timed or numpackets
# number of packets sent on each stream
set ::test::durationMode     numpackets
set ::test::numFrames_up     1000
set ::test::numFrames_down   1000

# Minimum packet size requirements:
# UDP with data integ & seq check enabled = 72 bytes
# TCP with data integ & seq check enabled = 84 bytes
# UDP with data integ & seq check disabled = 64 bytes down, 68 bytes up (so that vlan tag removal does not result in invalid packet size)
# TCP with data integ & seq check disabled = 74 bytes
set ::test::frameSize_up     256
set ::test::frameSize_down   256

# destination ip can be specified in one of 3 forms:
# single value: 1.2.3.4
# mask only used if IP is also defined
set ::test::destIp_up        ""
set ::test::destIp_down      ""

set ::test::hostname    10.10.100.3
# ports should be specified as {chassis card port}
set ::test::smtsPort    {0 0 0}
set ::test::smtsip      10.10.96.105
set ::test::smtsmask    255.255.255.224
set ::test::smtsgw      10.10.96.97
set ::test::smtsmac     "e0 01 04 00 00 00"

set ::test::smPort      {0 0 0}
set ::test::smip        0.0.0.0; # starting IP address
set ::test::smmask      255.255.0.0
set ::test::smgw        10.117.0.1

set ::test::useDefaultPortProperties 1
# the following variables are only used if useDefault above is 0
set ::test::smPhyMode         portPhyModeCopper
set ::test::smAutonegotiate   false
set ::test::smDuplexMode      full
set ::test::smLineSpeed       1000
set ::test::smtsPhyMode         portPhyModeCopper
set ::test::smtsAutonegotiate   true
set ::test::smtsDuplexMode      full
set ::test::smtsLineSpeed       1000



# Cloudshell Values! If reservationID is specified, this scirpt will query the passed in reservation for its ixia values.
#    If blueprintName is specified, this script will attempt to reserve the named blueprint, then set the reservationId.
set ::test::blueprintName  "NBN Port Pair"
set ::test::blueprintParameters [list]
set ::test::reservationId ""
set ::test::cloudshellTestMode false


# latency test modes:
#cutThrough - first data bit in to first data bit out
#storeAndForward - last data bit in to first data bit out
# jitter test mode:
#interArrivalJitter - inter-arrival jitter
# note: cannot test latency and jitter simultaneously
set ::test::latencyMode  cutThrough

set ::test::pollInterval    30; # time in seconds to wait after one stats collection before starting the next
set ::test::postTxDelay     20;  # time in seconds to wait after transmit complete before collecting stats

# flag to enable option to stop IXIA test if interval stats poll shows RX bps of 0 for any flow
set ::test::stopOnRxBpsZero 0

# Options include: dataIntegrity, sequence
#set ::test::packetValidation {dataIntegrity sequence}
set ::test::packetValidation {}

# flags to enable options to clear settings or disable ports at end of test script
set ::test::resetPortsToDefaultsAtEnd 1
set ::test::disablePortsAtEnd 0

# Throughput threshold values for the upstream, downstream
set ::test::ixOsUsTputThreshold 100
set ::test::ixOsDsTputThreshold 100

##Parameter indicates whether to use a incremental ramp up / seperate ramp up stream
## 1 -> incremental ramp up
## 0 -> seperate ramp up stream
set ::test::adjustRampup 0
array set ::test::rateArray {}
#\endverbatim

##
#@}

::TestLibrary::setup

# Set here as we dont want parameters to override them.
set ::test::blueprintReserved false
set ::test::ixiaConnected false

set ::TestLibrary::testName [file rootname [file tail [info script]]]

# have to move require of IxOsLibrary to after TestLibrary::setup
# so that we've read the lab info file and know what our IxOS version is
package require IxOsLibrary


##############
# Procedures #
##############
proc computeRateAverages { } {
    foreach name [array names ::test::rateArray] {
        set valueList $::test::rateArray($name)
        set mean [::math::statistics::mean $valueList]
        set ::test::rateArray($name) [expr {round($mean)}]
    }
}

# add all the commas
proc addCommas number {regsub -all {\d(?=(\d{3})+($|\.))} $number {\0,}}



##
#\brief get the interval statistics for the stream.
# Gets the statistics for a packet group in a streamId.
# Updates the Reda with the stats.
#\param stream the stream dictionary containing all the streams
#\param firstPoll tells if its the first poll
#\param finalCall whether to print to csv / not
#\return 0 if there was no through put failure
#\return count of stream that have a through put failure.
#
# <b>Example:</b>
#\code [getIntervalStats upStreamList 1] \endcode
# <b>Signature:</b>
#\code
proc getIntervalStats {stream firstPoll {finalCall 0}} {
#\endcode

    global rateHandle
    global verification_calls

    # initialize flag to detect tput failure of rxbps dropping to 0
    set tputFailure 0

    set statList {totalFrames totalByteCount bitRate frameRate averageLatency minLatency maxLatency}
    set varNameList {txpkts txdur rxpkts rxbytes rxbps rxfps avglat minlat maxlat}

    # default all validation checking to off
    set validationEnable(dataIntegrity) 0
    set validationEnable(sequence) 0
    foreach validationType $::test::packetValidation {
        set validationEnable($validationType) 0
    }
    if {$validationEnable(sequence)} {
        lappend statList smallSequenceError bigSequenceError reverseSequenceError totalSequenceError sequenceGaps duplicateFrames
        lappend varNameList smseqerr bigseqerr revseqerr totseqerr seqgaps dupframes
    }


    set rateVerification [list]
    dict with stream {
        if {$direction eq "up"} {
            set stats [::IxOsLibrary::getPacketGroupStats $streamId $::test::smPort $::test::smtsPort $statList]
        } else {
            set stats [::IxOsLibrary::getPacketGroupStats $streamId $::test::smtsPort $::test::smPort $statList]
        }


        set numUsers 0
        set streamFailureFs "| %10s | %4s | %4s | %5s |"
        set title [format $streamFailureFs Direction stag ctag cpbit]
        set sep [string repeat - [string length $title]]
        set failureString "\t$sep\n"
        append failureString "\t$title\n"
        append failureString "\t$sep\n"
        set streamFailures 0
        foreach $varNameList $stats {
            if {$direction eq "up"} {
                set trafficDirection upstream
            } else {
                set sVlan [dict get $svlan id]
                set trafficDirection downstream
            }
            if {[dict get $svlan step]} {
                set sVlan [expr {[dict get $svlan id] + $numUsers}]
            } else {
                set sVlan [dict get $svlan id]
            }
            if {[dict get $cvlan step]} {
                set cVlan [expr {[dict get $cvlan id] + $numUsers}]
            } else {
                set cVlan [dict get $cvlan id]
            }

            set cpcpBit [dict get $cvlan pcpBit]
            set spcpBit [dict get $svlan pcpBit]
            set pcpBit $cpcpBit

            if {$ceMode && $direction eq "down"} {set pcpBit $spcpBit}

            set data "[clock format [clock seconds] -format "%D %T"],$sVlan,$cVlan,$pcpBit,$direction,"
            foreach varName $varNameList {append data [set $varName],}
            puts $rateHandle $data

            if {!$finalCall} {
                if {!$firstPoll} {
                    # first poll is always 0, dont add it to rates.
                    lappend ::test::rateArray($sVlan,$cVlan,$pcpBit) $rxbps
                }
            }
            if {$finalCall} {
                if {![info exists ::test::rateArray($sVlan,$cVlan,$pcpBit)]} {
                    set ::test::rateArray($sVlan,$cVlan,$pcpBit) 0
                }
                set averageRate $::test::rateArray($sVlan,$cVlan,$pcpBit)
                switch -- $expPassType {
                    "rate" {
                        set thisName ${sVlan}-${cVlan}-${pcpBit}

                        # Compute difference of actual rate to expected rate
                        if {$expPassRate == 0} {
                            # expPassRate is 0
                            set difference 0
                            if {$averageRate != 0} {set difference 100}

                        } else {
                            # Standard case, pass rate is non 0
                            set difference [expr {double($averageRate) / double($expPassRate)}]
                            if {$difference > 1} {
                                # Greater than! ie, 1.2. Subract 1 for .2
                                set difference [expr {$difference - 1.0}]
                            } else {
                                # Less than! .8, then 1 -.8 for .2
                                set difference [expr {1.0 - $difference}]
                            }
                            set difference [expr {$difference * 100}]
                        }

                        dict set rateVerification $thisName [dict create\
                            direction $direction\
                            sVlan $sVlan\
                            cVlan $cVlan\
                            pcpBit $pcpBit\
                            actRate $averageRate\
                            expRate $expPassRate\
                            difference [format "%.2f" $difference]\
                        ]
                    }
                    default { }
                }
                if {$txpkts == 0} {
                    set tput 0
                    set loss 0
                } else {
                    set tput [mpexpr ($rxpkts.0/$txpkts)*100]
                    set loss [mpexpr double($txpkts - $rxpkts)/$txpkts]
                }
                set rxmbits [mpexpr ($rxbytes/1000000.0)*8]
                set line [format "%s,%s,%s,%s,%s,%s,%s,%s,%.2f%%,%.8f,%.2E,%s,%s,%s"\
                          $trafficDirection\
                          $name\
                          $sVlan\
                          $cVlan\
                          $pcpBit\
                          $txpkts\
                          $rxpkts\
                          $averageRate\
                          $tput\
                          $loss\
                          $loss\
                          [expr {$avglat/1000000.0}]\
                          [expr {$minlat/1000000.0}]\
                          [expr {$maxlat/1000000.0}]]
                if {$validationEnable(sequence)} {
                    append line [format ",%s,%s,%s,%s,%s,%s"\
                          $smseqerr\
                          $bigseqerr\
                          $revseqerr\
                          $totseqerr\
                          $seqgaps\
                          $dupframes]
                }
                puts $::TestLibrary::csv $line
            } elseif {[string first "18446744" $rxfps] >= 0} {
                # negative FrameRate problem. Drop the statistics this poll and continue.
            } elseif {($rxbps == 0) && ($txpkts > 0) && !$firstPoll} {
                if {$expPassType eq "rate"} {
                    if {$expPassRate != 0} {
                        # If we expect a rate of zero, dont print anything :)
                        incr tputFailure
                        incr streamFailures
                        append failureString "\t[format $streamFailureFs $trafficDirection $sVlan $cVlan $pcpBit]\n"
                    }
                } else {
                    incr tputFailure
                    incr streamFailures
                    append failureString "\t[format $streamFailureFs $trafficDirection $sVlan $cVlan $pcpBit]\n"
                }
            }
            incr numUsers
      } ;# End of foreach

    # Analyze the results
    if {($rateVerification != [list]) && $finalCall} {
        set failedModems [list]
        set rateList     [list]
        set ratePF pass
        set streamName [dict get $stream name]-${trafficDirection}-${pcpBit}

        write "(Rate VP) Values for $streamName:\n[::ChartLibrary::chartDict $rateVerification]" debug
        foreach key [dict keys $rateVerification] {lappend rateList [dict get $rateVerification $key actRate]}
        write "(Rate VP) Statistical Variance of rates: --[::math::statistics::var $rateList]--" debug
        write "(Rate VP) Standard Deviation of rates:   --[::math::statistics::stdev $rateList]--" debug
        write "(Rate VP) (stag-ctag-pcp) Checking results: (lowerLimit <= Actual) && (Actual <= upperLimit)" debug
        foreach key [dict keys $rateVerification] {
            set direction [dict get $rateVerification $key direction]
            set tolerance .05
            if {$direction eq "up"} { set tolerance .07}
            set expRate    [dict get $rateVerification $key expRate]
            set actRate    [dict get $rateVerification $key actRate]
            set difference [dict get $rateVerification $key difference]

            # Get us limits on +-$tolerance
            set upper [expr {round((1 + $tolerance) * $expRate)}]
            set lower [expr {round((1 - $tolerance) * $expRate)}]
            write "(Rate VP) ($key) Check: ($lower <= $actRate) && ($actRate <= $upper). Tolerance: $tolerance" debug

            if {($lower <= $actRate) && ($actRate <= $upper)} {
                # Passes! Rate was within expected bounds.
            } else {
                # Failed. How bad?
                if {$expRate == 0} {
                    # Expected 0, got $actRate.
                    set reason "Expected a rate of 0, received $actRate"
                    lappend failedModems [list $key $reason]
                    set ratePF fail
                    continue
                }
                set reason "$key expected $expRate b/s, received $actRate bps: Difference of ${difference}%."
                lappend failedModems [list $key $reason]
                set ratePF fail
            }
        }

        # Set the pass/fail reasons
        if {$ratePF eq "pass"} {
            set reason "All modems in stream $streamName met the expected rate."
        } else {
            foreach fail $failedModems {
                lassign $fail key reason
                write "(Rate VP) $streamName: Modem $key failed: $reason" warn
            }
            set reason [format "%3s modems in stream %s failed to reach rate of %s" [llength $failedModems] $streamName $expRate]
        }
        lappend verification_calls [list $reason $ratePF]
    }

      if {$streamFailures} {
          append failureString "\t$sep\n"
          set streamName [dict get $stream name]-${trafficDirection}-${pcpBit}
          write "$streamName: $streamFailures out of $repeat received 0 b/s:\n$failureString" error
      }
    } ;# End of dict with
    dict for {n v} $stream {unset $n}
    return $tputFailure
}

##
#\brief get the stream statistics for the stream.
# Gets the statistics for all the packet groups in a streamId.
# Updates the Reda with the stats.
#\param streamList list of all streams, each stream is a dictionary.
#\param trafficDirection whether upstream or downstream
#\return None
#\attention Write any warnings/preconditions here.
#
# <b>Example:</b>
#\code [getStreamStats upStreamList upstream] \endcode
# <b>Signature:</b>
#\code
proc getStreamStats {streamList trafficDirection} {
#\endcode

    global tput_up
    global tput_down
    global verification_calls

    set header [format "%-14s %-10s %-12s %-12s %-10s %-10s %-9s %-11s %-17s" "Group Name" "SVlan ID" "CVlan ID" "PCP(CVlan)" "Tx Frames" "Rx Frames" "Tput" "Loss" "Loss(scientific)"]
    write "$header"

    set statList { {totalFrames {total}} {totalByteCount {total}} {averageLatency {avg}} {minLatency {min}} {maxLatency {max}} }
    set varNameList {txpkts txdur rxpkts rxbytes avglat minlat maxlat}

    # default all validation checking to off
    set validationEnable(dataIntegrity) 0
    set validationEnable(sequence) 0
    foreach validationType $::test::packetValidation {
        set validationEnable($validationType) 1
    }
    if {$validationEnable(sequence)} {
        lappend statList {smallSequenceError {total}} {bigSequenceError {total}} {reverseSequenceError {total}} {totalSequenceError {total}} {sequenceGaps {total}} {duplicateFrames {total}}
        lappend varNameList smseqerr bigseqerr revseqerr totseqerr seqgaps dupframes
    }

    set txTotalTput 0
    set rxTotalTput 0
    set writeLine ""


    # List of dictionaries to hold rate validation dictionaries
    set rateVerification [dict create]

    foreach stream $streamList {
        dict with stream {
           set streamId [dict get $stream streamId]
           if {$trafficDirection eq "upstream"} {
              set stats [::IxOsLibrary::getAggPacketGroupStats $streamId $::test::smPort $::test::smtsPort $statList]
              if {$validationEnable(dataIntegrity)} {
                  set diframes [::IxOsLibrary::getStat $::test::smtsPort dataIntegrityFrames]
                  set dierrors [::IxOsLibrary::getStat $::test::smtsPort dataIntegrityErrors]
              }
           } else {
              set stats [::IxOsLibrary::getAggPacketGroupStats $streamId $::test::smtsPort $::test::smPort $statList]
              if {$validationEnable(dataIntegrity)} {
                  set diframes [::IxOsLibrary::getStat $::test::smPort dataIntegrityFrames]
                  set dierrors [::IxOsLibrary::getStat $::test::smPort dataIntegrityErrors]
              }
           }
           # will only loop once since we are only requesting one stream's worth of stats
           foreach $varNameList $stats {

               if {$txpkts == 0} {
                   set tput 0
                   set loss 0
               } else {
                   set tput [mpexpr ($rxpkts.0/$txpkts)*100]
                   set loss [mpexpr double($txpkts - $rxpkts)/$txpkts]
               }

               set rxmbits [mpexpr ($rxbytes/1000000.0)*8]

                set sVlan   "[dict get $svlan id]"
                set cVlan   "[dict get $cvlan id]"
                set cpcpBit "[dict get $cvlan pcpBit]"
                set spcpBit "[dict get $svlan pcpBit]"
                set pcpBit $cpcpBit
                if {$ceMode && $direction eq "down"} {set pcpBit $spcpBit }


                # Needed in the case where a user quits test before data is generated
                if {![info exists ::test::rateArray($sVlan,$cVlan,$pcpBit)]} {
                    set ::test::rateArray($sVlan,$cVlan,$pcpBit) 0
                }

                set averageRate $::test::rateArray($sVlan,$cVlan,$pcpBit)

               set line [format "%-14s %-10s %-12s %-12s %-10s %-10s %-8.2f%%  %-11.8f %-17.2E"\
                         $name\
                         $sVlan\
                         $cVlan\
                         $pcpBit\
                         $txpkts\
                         $rxpkts\
                         $tput\
                         $loss\
                         $loss]
               write "$line"

               set line [format "%s,%s,%s,%s,%s,%s,%s,%s,%.2f%%,%.8f,%.2E,%s,%s,%s"\
                         $trafficDirection\
                         "Group"\
                         $sVlan\
                         $cVlan\
                         $pcpBit\
                         $txpkts\
                         $rxpkts\
                         "-"\
                         $tput\
                         $loss\
                         $loss\
                         [expr {$avglat/1000000.0}]\
                         [expr {$minlat/1000000.0}]\
                         [expr {$maxlat/1000000.0}]]
               if {$validationEnable(sequence)} {
                append line [format ",%s,%s,%s,%s,%s,%s"\
                         $smseqerr\
                         $bigseqerr\
                         $revseqerr\
                         $totseqerr\
                         $seqgaps\
                         $dupframes]
               }

               lappend writeLine $line
                set name ""
                switch -- $expPassType {
                    "oldrate" {
                        set name "${direction}Total_bps--$sVlan/$cVlan/$pcpBit"
                        set expectedRate $expPassRate
                        set upper_modifier .025
                        set lower_modifier .025

                        if {$expPassRate == -1} {
                            set name "${direction}--$sVlan/$cVlan/$pcpBit--DEFAULT_PASS"
                            lappend verification_calls [list $name pass int < $expPassRate $averageRate]
                            continue
                        }


                        # Mpss does rates based on OTA values, IE, with the S-tag and crc stripped.
                        # The MPSS adds 1% more onto FL flows
                        if {$direction eq "down" && ([dict get $rate value] > $expPassRate)} {
                            set upper_modifier .03
                            set expectedRate [expr {$expPassRate * (double($framesize)/(double($framesize) - 8))}]
                            write "Changing upper tolerance to 3%, modifying expected rate to $expectedRate to account for missing s-tag"

                        } elseif {$direction eq "up" && ([dict get $rate value] < 1000000) && ([dict get $rate value] >= $expPassRate)} {
                            # MPSS scheduling granularity of 32 bits, the modem/mpss can't properly scedule when the rates are this low.
                            set upper_modifier .04
                            write "Chaning upper tolerance to 4% to account for mpss rl granularity."

                        }


                        # 1% variance is acceptable in our rates...
                        set lowerRate  [expr {$expPassRate  * (1 - $lower_modifier)}]
                        set higherRate [expr {$expectedRate * (1 + $upper_modifier)}]

                        write "Adjusted rate: $expectedRate"
                        write "Expected rate: $lowerRate --  $expPassRate -- $higherRate"
                        write "Actual  rate: --$averageRate--"
                        set varience [expr {100 - (100 * (double($averageRate) / $expectedRate))}]
                        set varience [expr {abs($varience)}]
                        write "[format "%.2f" $varience]% off of expected value of $expectedRate or $expPassRate"
                        if {($lowerRate <= $averageRate) && ($higherRate >= $averageRate)} {
                            lappend verification_calls [list $name pass int >= $expPassRate $averageRate]
                        } else {
                            lappend verification_calls [list $name fail int < $expPassRate $averageRate]
                        }

                    }

                    "throughput" {
                        set name "${direction}TotalTput--$sVlan/$cVlan/$pcpBit"
                        if {$tput >= $expPassRate} {
                            lappend verification_calls [list $name pass int >= $expPassRate $tput]

                        } else {
                            lappend verification_calls [list  $name fail int < $expPassRate $tput]
                        }
                    }
                    default { }
                }
           }
       }


        set txTotalTput [expr {$txTotalTput + $txpkts}]
        set rxTotalTput [expr {$rxTotalTput + $rxpkts}]
        dict for {n v} $stream {unset $n}
    }


    # Set up the "ALL" line
    if {$txTotalTput == 0} {
        set tput 0
        set loss 0
    } else {
        set tput [mpexpr ($rxTotalTput.0/$txTotalTput)*100]
        set loss [mpexpr double($txTotalTput - $rxTotalTput)/$txTotalTput]
    }

    if {$trafficDirection eq "upstream"} {
        set txport $::test::smPort
        set rxport $::test::smtsPort
        set tput_up $tput
    } else {
        set txport $::test::smtsPort
        set rxport $::test::smPort
        set tput_down $tput
    }

    set line [format "%s,%s,%s,%s,%s,%s,%s,%s,%.2f%%,%.8f,%.2E,,,,"\
              $trafficDirection\
              "ALL"\
              "ALL"\
              "ALL"\
              "ALL"\
              $txTotalTput\
              $rxTotalTput\
              "-"\
              $tput\
              $loss\
              $loss]

    if {$validationEnable(sequence)} {
        append line ",,,,,,"
        if {$validationEnable(dataIntegrity)} {
            # dataIntegrity with after sequence
            append line [format ",%s,%s"\
                [::IxOsLibrary::getStat $rxport dataIntegrityFrames]\
                [::IxOsLibrary::getStat $rxport dataIntegrityErrors]]
        }
    } elseif {$validationEnable(dataIntegrity)} {
        # dataIntegrity with no sequence
        append line [format ",,,,,,,%s,%s"\
                [::IxOsLibrary::getStat $rxport dataIntegrityFrames]\
                [::IxOsLibrary::getStat $rxport dataIntegrityErrors]]
    }
    puts $::TestLibrary::csv $line
    foreach line $writeLine {
        puts $::TestLibrary::csv $line
    }

    return
}

# Wraps all the disconnect stuff!
proc disconnect { } {
    if {$::test::blueprintReserved} {
        if {![::CloudshellLibrary::endSandbox $::test::reservationId]} {
            write "Error ending sandbox with reservation id --$::test::reservationId--" error
        }
    }

    if {$::test::ixiaConnected} {
        ::IxOsLibrary::disconnect
    }
}


set rateHandle ""

##
#\brief run proc
#\private
proc run { } {
    # test setup globals
    global runMode
    global sigquit
    global tput_up
    global tput_down
    global verification_calls
    global switchDataTrafficStats
    global rateHandle

    write "Starting the Ixia L2 Script!"
    if {($::test::reservationId ne "") || ($::test::blueprintName ne "")} {
        # Asked for a reservation, but missing cloudshell values.
        if {![info exists ::station::cloudshell_user] ||\
            ![info exists ::station::cloudshell_pass] ||\
            ![info exists ::station::cloudshell_domain]} {

                ::TestLibrary::saveCriteriaResult GetCloudShellReservation FAIL
                write "Missing Cloudshell values (user/pass/domain) needed to make a reservation." error
                return False
        }

        set credentials [list $::station::cloudshell_user\
                              $::station::cloudshell_pass\
                              $::station::cloudshell_domain]


        # If we have a blueprint name, we need to go and reserve a blueprint!
        if {$::test::blueprintName ne ""} {
            set sandboxName "SAM-Ixia-L2-local-[clock seconds]-$::TestLibrary::stepId"
            if {$::TestLibrary::runId != 0} {
                set sandboxName "SAM-Ixia-L2-reda-$::TestLibrary::runId-$::TestLibrary::stepId"
            }
            set duration [::CloudshellLibrary::convertToIso8601 $::test::hours [expr {$::test::minutes + 20}] $::test::seconds]
            set blueprintValues [dict create name        $::test::blueprintName\
                                             duration    $duration\
                                             sandboxName $sandboxName\
                                             params      $::test::blueprintParameters]


            set reservation [::CloudshellLibrary::ReserveCloudshellBlueprint $credentials $blueprintValues]
            if {$reservation == [list]} {
                write "Failed to reserve a cloudshell blueprint with name $::test::blueprintName." error
                ::TestLibrary::saveCriteriaResult GetCloudShellReservation FAIL
                return False
            }
            set ::test::reservationId [dict get $reservation id]
            set ::test::blueprintReserved true
        }

        # A reservation ID means we need to extract values
        if {$::test::reservationId ne ""} {
            set values [::CloudshellLibrary::GetSandBoxIxiaValues $::test::reservationId $credentials]
            if {$values == [list]} {
                write "Unable to get values properly from cloudshell" error
                ::TestLibrary::saveCriteriaResult GetCloudShellReservation FAIL
                return False
            }
            write "--$values--"
            # Set the values from cloudshell!
            lassign [split [dict get $values server ip] "/"] ::test::serverip ::test::servermask

            set ::test::servergw [dict get $values server gw]
            set ::test::serverPort [dict get $values server port]
            set ::test::clientPort [dict get $values client port]

            if {[dict get $values [dict get $values server chassis] address] eq [dict get $values [dict get $values client chassis] address]} {
                set ::test::hostname  [dict get $values [dict get $values client chassis] address]
            } else {
                # Server port always comes first in cloudshell (convention only), so we set it to index one
                set ::test::hostname [list [dict get $values [dict get $values server chassis] address]\
                                           [dict get $values [dict get $values client chassis] address]]
            }

        }
        # At this point, we have each ::test:: namespace value configured correctly for ixia.
        ::TestLibrary::saveCriteriaResult GetCloudShellReservation pass

        # Big boy mode. We have a valid reservation, so we dont care whose on the port its ours.
        set ::IxOsLibrary::forceTakeOwnership force

        # Ensure these dont override
        set ::test::serverIpGwLookup false

        #############
        # THe above is unchanged (mostly) from L3. This is where we make em match
        ############
        set ::test::smPort $::test::clientPort
        set ::test::smtsPort $::test::serverPort


    }


    if {$::test::cloudshellTestMode} {
        ############################################################
        # KWREN
        ############################################################
        # Just for testing, temporary
        ::cron::every cs_sigquit_check 2 {
            if {$::sigquit} {
                set ::done 1
            }
        }
        write "Temporary mode for testing cloudshell"

        write "Important Values:"
        write "\t::test::serverip --$::test::serverip--"
        write "\t::test::servermask --$::test::servermask--"
        write "\t::test::servergw --$::test::servergw--"
        write "\t::test::serverPort --$::test::serverPort--"
        write "\t::test::clientPort --$::test::clientPort--"
        write "\t::test::hostname --$::test::hostname--"


        after 360000 {set ::done 1}
        vwait ::done
        write "Ending sandbox:"
        disconnect
        return True
        ############################################################
    }


    set varNameList {txpkts txdur rxpkts rxbytes rxbps rxfps avglat minlat maxlat}
    lappend varNameList smseqerr bigseqerr revseqerr totseqerr seqgaps dupframes

    # Rates CSV
    set header "Timestamp,Svlan,Cvlan,Pcpbit,Direction,"
    set rateFileName "$::TestLibrary::resDir/${::TestLibrary::testName}_rates_${::TestLibrary::runId}_${::TestLibrary::stepId}.csv"
    foreach name $varNameList {append header $name,}
    set rateHandle [open $rateFileName w]
    puts $rateHandle $header


    # Validate the user dictionary, so we dont have to error check later. validateDictionary returns 1 on
    # failure.
    set ::test::streams [::IxOsLibrary::validateDictionary $::test::streams]
    if {$::test::streams == 1 } {
        write "Validation failure. Exiting..." error
        return 1
    }

    # Convert the user dictionary into more Ixia friendly streams.
    set streamList [::IxOsLibrary::convertDictionary $::test::streams]
    # Configure ports into advanced mode, set port macs.
    if {![::TestLibrary::docsOnly]} {

        # Configure ports
        if {![::IxOsLibrary::connect $::test::hostname]} {
            ::TestLibrary::saveCriteriaResult "Unable to establish connection to ixia chassis" fail
            return False
        }
        set ::test::ixiaConnected True
        if {![::IxOsLibrary::addPorts $::test::smPort $::test::smtsPort]} {
            ::TestLibrary::saveCriteriaResult "Unable to reserve Ports" fail
            return False
        }

        # Configure ports
        if {$::test::useDefaultPortProperties} {::IxOsLibrary::setPortMode}
        ::IxOsLibrary::configPort $::test::smPort SM portTxModeAdvancedScheduler "" $::test::smmac\
                                  $::test::smgw $::test::smmask $::test::smAutonegotiate\
                                  $::test::smDuplexMode $::test::smLineSpeed $::test::smPhyMode\
                                  0 0 0 0  $::test::packetValidation

        ::IxOsLibrary::configPort $::test::smtsPort SMTS portTxModeAdvancedScheduler $::test::smtsip\
                                  $::test::smtsmac $::test::smtsgw $::test::smtsmask $::test::smtsAutonegotiate\
                                  $::test::smtsDuplexMode $::test::smtsLineSpeed $::test::smtsPhyMode 0 0 0 0\
                                  $::test::packetValidation

        ::IxOsLibrary::writePortsToHw
    }


    write "Configured Client Side port: $::test::smPort"
    write "Configured Server Side port: $::test::smtsPort"


    if {$::test::durationMode == "numpackets"} {
        set flowType stopStream
        set durationStr "$numFrames_up frames"
    } else {
        set flowType contPacket
        set durationStr "$::test::hours h $::test::minutes m $::test::seconds s"
    }

    set newList {}

    #for rampup and ramp down.
    set up_rampList {}
    set down_rampList {}
    set down_streams {}
    set up_streams {}

    set title [list "Direction" "Vlans (s/c)" "Priority" "Count" "Rate Per Modem" "Rate" "Packet Size" "Duration" "File"]
    foreach stream $streamList {
        if {[dict get $stream direction] eq "up"} {
            if {![::TestLibrary::docsOnly]} {
                if {$::test::adjustRampup} {
                    set streamId [::IxOsLibrary::configL2Upstream $::test::smPort $::test::smtsPort $flowType $stream $::test::packetValidation $::test::adjustRampup 0]
                } else {
                    set rampId   [::IxOsLibrary::configL2Upstream $::test::smPort $::test::smtsPort $flowType $stream $::test::packetValidation $::test::adjustRampup 1]
                    set streamId [::IxOsLibrary::configL2Upstream $::test::smPort $::test::smtsPort $flowType $stream $::test::packetValidation $::test::adjustRampup 0]
                    lappend up_streams $streamId
                    lappend up_rampList $rampId
                }
            }

            # create upstream results table entry
            set sVlan [dict get [dict get $stream svlan] id]
            set cVlan [dict get [dict get $stream cvlan] id]
            set repeat [dict get $stream repeat]
            set protocol [dict get [dict get $stream protocol] type]
            set framesize [dict get $stream framesize]
            set srcPort [dict get [dict get $stream protocol] srcPort]
            set destPort [dict get [dict get $stream protocol] destPort]
            set rate  [dict get [dict get $stream rate] value]
            set rateMode  [dict get [dict get $stream rate] mode]
            set ratePerModem  [dict get [dict get $stream rate] perModem]
            set pcpBit  [dict get [dict get $stream cvlan] pcpBit]
            if {[llength $framesize] > 2} {
                set frameSizeStr "weighted distribution: (weight(count)@size) "
                foreach {size weight} $framesize {
                    append frameSizeStr "$weight@$size "
                }
            } elseif {[llength framesize] == 2} {
                set frameSizeStr "size range $framesize"
            } else {
                set frameSizeStr "size $framesize"
            }

            if {$ratePerModem} {
                set rateValuePerModem $rate
                set rateAgg [expr {$rate * $repeat}]
            } else {
                set rateValuePerModem [expr {$rate / $repeat}]
                set rateAgg  $rate
            }

            lappend stepOutput  [list "Up" "$sVlan/$cVlan" $pcpBit $repeat [addCommas $rateValuePerModem] [addCommas $rateAgg] $framesize $durationStr [file tail [info script]]]
        } else {

            if {![::TestLibrary::docsOnly]} {
                if {$::test::adjustRampup} {
                    set streamId [::IxOsLibrary::configL2Downstream $::test::smtsPort $::test::smPort $flowType $stream $::test::packetValidation $::test::adjustRampup 0]
                } else {
                    set rampId   [::IxOsLibrary::configL2Downstream $::test::smtsPort $::test::smPort $flowType $stream $::test::packetValidation $::test::adjustRampup 1]
                    set streamId [::IxOsLibrary::configL2Downstream $::test::smtsPort $::test::smPort $flowType $stream $::test::packetValidation $::test::adjustRampup 0]
                    lappend down_streams $streamId
                    lappend down_rampList $rampId
                }
            }

            # create downstream results table entry
            set sVlan [dict get [dict get $stream svlan] id]
            set cVlan [dict get [dict get $stream cvlan] id]
            set repeat [dict get $stream repeat]
            set protocol [dict get [dict get $stream protocol] type]
            set framesize [dict get $stream framesize]
            set srcPort [dict get [dict get $stream protocol] srcPort]
            set destPort [dict get [dict get $stream protocol] destPort]
            set rate  [dict get [dict get $stream rate] value]
            set rateMode  [dict get [dict get $stream rate] mode]
            set ratePerModem  [dict get [dict get $stream rate] perModem]
            set pcpBit  [dict get [dict get $stream cvlan] pcpBit]
            if {[llength $framesize] > 2} {
                set frameSizeStr "weighted distribution: (weight(count)@size) "
                foreach {size weight} $framesize {
                    append frameSizeStr "$weight@$size "
                }
                set frameSizeStr [string trim $frameSizeStr]
            } elseif {[llength framesize] == 2} {
                set frameSizeStr "size range $framesize"
            } else {
                set frameSizeStr "size $framesize"
            }
            if {$ratePerModem} {
                set rateValuePerModem $rate
                set rateAgg [expr {$rate * $repeat}]
            } else {
                set rateValuePerModem [expr {$rate / $repeat}]
                set rateAgg  $rate
            }
            lappend stepOutput  [list "Down" "$sVlan/$cVlan" $pcpBit $repeat [addCommas $rateValuePerModem] [addCommas $rateAgg] $framesize $durationStr [file tail [info script]]]
        }
        if {![::TestLibrary::docsOnly]} {
            dict set stream streamId $streamId
            lappend newList $stream
        }
    }


    write "Configure Ixia for the following streams:\n[::ChartLibrary::chartList $title $stepOutput]"

    set streamList $newList

    if {[::TestLibrary::docsOnly]} { return }
    ::IxOsLibrary::writeConfigToHw

    if {$::test::adjustRampup} {
        ::IxOsLibrary::clearStats
        if {$::test::upstreamEnable}   {::IxOsLibrary::startPacketGroups $::test::smtsPort}
        if {$::test::downstreamEnable} {::IxOsLibrary::startPacketGroups $::test::smPort}
    }

    if {$::test::upstreamEnable} {
        write "Starting upstream transmit."
        if {[::IxOsLibrary::startTransmit $::test::smPort]} {
            write "Error starting upstream transmit." error
        }
    }

    if {$::test::downstreamEnable} {
        write "Starting downstream transmit."
        if {[::IxOsLibrary::startTransmit $::test::smtsPort]} {
            write "Error starting downstream transmit." error
        }
    }


    if {$::test::adjustRampup} {
        #Incremental ramp up
        after 2000 {set ::rampInterval 1}
        vwait ::rampInterval
        write "(Ramp up) (adjust) Ramping up streams..." debug
        # Fancy Ramp up here!
        set rampingStreams $streamList
        while {[llength $rampingStreams] != 0} {
            set updates ""

            # We iterate over each stream not yet at its goal rate yet
            foreach stream $rampingStreams {
                set goalRate [dict get [dict get $stream rate] value]

                # Double the rate, then check if we have passed the goal.
                # If we have, set the stream rate to the goal and drop from the loop
                set currentRate [expr 2 * [dict get $stream currentRate]]
                if {$currentRate >= $goalRate} {
                    write "(Ramp up) (adjust) Stream [dict get $stream streamId] ([dict get $stream direction]) is at the configured rate." debug
                    ::IxOsLibrary::L2adjust $stream $goalRate
                } else {
                    write "(Ramp up) (adjust) Stream [dict get $stream streamId] ([dict get $stream direction]) is at rate $currentRate." debug
                    dict set stream currentRate $currentRate
                    ::IxOsLibrary::L2adjust $stream $currentRate
                    lappend updates $stream
                }
                # Give ixia a break between streams. Adds a large chunk to run time, but ixia needs time.
                after 1000
            }
            set rampingStreams $updates
        }
    } else {
        ## Seperate ramp up stream
        #######################################################
        # Packet group IDs come after we get traffic flowing
        #######################################################
        # start transmission in enabled directions
        write "(Ramp up) Running 10s ramp up time..."
        after 10000 {set ::rampInterval 1}
        vwait ::rampInterval

        ::IxOsLibrary::clearStats
        if {$::test::upstreamEnable}   {::IxOsLibrary::startPacketGroups $::test::smtsPort}
        if {$::test::downstreamEnable} {::IxOsLibrary::startPacketGroups $::test::smPort}

        write "(Ramp up) Suspending ramp up streams..." debug
        ::IxOsLibrary::L2Suspend $::test::smPort $up_rampList
        ::IxOsLibrary::L2Suspend $::test::smtsPort $down_rampList

        write "(Ramp up) Starting main streams..." debug
        ::IxOsLibrary::L2Resume $::test::smPort $up_streams
        ::IxOsLibrary::L2Resume $::test::smtsPort $down_streams

    }
    write "(Ramp up) Complete. Starting test." debug
    write "Starting configured transmission..."

    set upStreamList {}
    set downStreamList {}
    foreach stream $streamList {
        if {[dict get $stream direction] eq "up"} {
           lappend upStreamList $stream
        } else {
           lappend downStreamList $stream
        }
    }


    set initialSeconds [clock seconds]
    set firstLoop 1
    set firstPoll 1
    set lastPoll 0
    set tputFailure 0

    if {$::test::durationMode eq "timed"} {

        # convert wait time to seconds
        set totalSeconds [expr $::test::seconds + ($::test::minutes * 60) + ($::test::hours * 3600)]
        set lastHour 0
        set lastMin 0

        while {1} {
            # Evaluate termination conditions (sigquit | time elapsed)
            if { $sigquit } {
                    write "SIGQUIT received - ending test"
                    break
            }

            if {![::TestLibrary::runStatus]} { break }

            # wait 1 seconds for each loop
            set ::IxOsLibrary::timedLoopFlag 1
            after 1000 {set ::IxOsLibrary::timedLoopFlag 0}

            # print current time status
            set currentSec  [expr {[clock seconds] - $initialSeconds}]
            set currentMin  [expr {int($currentSec / 60)}]
            set currentHour [expr {int($currentSec / 3600)}]
            if {$currentHour > $lastHour} {
                write "- hour $currentHour complete"
                incr lastHour
            } elseif {$::test::hours == 0 && $currentMin > $lastMin} {
                write "- minute $currentMin complete"
                incr lastMin
            }
            # check if time is up
            if {$currentSec >= $totalSeconds} {
                ::IxOsLibrary::stopTransmit
                break
            }

            set tputFailure 0
            if {$currentSec >= [expr {$lastPoll + $::test::pollInterval}]} {
                foreach stream $upStreamList {
                    set repeat [dict get $stream repeat]
                    if {[getIntervalStats $stream $firstPoll] == $repeat} {
                        write "Upstream tput failed on pcp [dict get [dict get $stream cvlan] pcpBit] stream for all $repeat UTs" warn
                        incr tputFailure
                    }
                }
                foreach stream $downStreamList {
                    set repeat [dict get $stream repeat]
                    if {[getIntervalStats $stream $firstPoll] == $repeat} {
                        write "Downstream tput failed on pcp [dict get [dict get $stream cvlan] pcpBit] stream for all $repeat UTs" warn
                        incr tputFailure
                    }
                }
                set lastPoll $currentSec

                # check if tput failure case
                if {$::test::stopOnRxBpsZero && $tputFailure > 0} {
                    write "Throughput failure case, stopping test" error
                    ::IxOsLibrary::stopTransmit
                    break
                }

                set firstPoll 0
                write "--------------- End of polling Interval. ---------------"
            }

            # finish waiting
            if {$::IxOsLibrary::timedLoopFlag} {
                vwait ::IxOsLibrary::timedLoopFlag
            }
        }
    } elseif {$::test::durationMode eq "numpackets"} {

        while {1} {
            # Evaluate termination conditions (sigquit | time elapsed)
            if { $sigquit } {
                    write "SIGQUIT received - ending test"
                    break
            }
            if {![::TestLibrary::runStatus]} { break }

            # check if test was completed
            if {[::IxOsLibrary::transmitting] == 0} { break }

            # wait 2 seconds for each loop
            after 2000 {set done iter}

            set tputFailure 0

            # check if time to collect stats
            set currentSec [expr {[clock seconds] - $initialSeconds}]
            if {!$firstLoop && $currentSec >= [expr {$lastPoll + $::test::pollInterval}]} {
                foreach stream $upStreamList {
                    set repeat [dict get $stream repeat]
                    if {[getIntervalStats $stream $firstPoll] == $repeat} {
                        write "Upstream tput failed on pcp [dict get [dict get $stream cvlan] pcpBit] stream for all $repeat UTs"
                        incr tputFailure
                    }
                }
                foreach stream $downStreamList {
                    set repeat [dict get $stream repeat]
                    if {[getIntervalStats $stream $firstPoll] == $repeat} {
                        write "Downstream tput failed on pcp [dict get [dict get $stream cvlan] pcpBit] stream for all $repeat UTs"
                        incr tputFailure
                    }
                }
                set lastPoll $currentSec

                # check if tput failure case
                if {$::test::stopOnRxBpsZero && $tputFailure > 0} {
                    write "Throughput failure case, stopping test"
                    ::IxOsLibrary::stopTransmit
                    break
                }

                set firstPoll 0
            }

            # finish waiting
            vwait done
            set firstLoop 0
        }
    } else {
        set tputFailure 0
        set firstPoll 0
        write "Invalid duration mode: $::test::durationMode" error
    }

    # if test not stopped yet, stop it
    if {[::IxOsLibrary::transmitting] != 0} {
        ::IxOsLibrary::stopTransmit
    }

    write "Transmit complete."
    ::TestLibrary::closeStopWindow

    # if we hit the tput failure case, don't bother waiting, just collect results asap
    if {$::test::stopOnRxBpsZero && $tputFailure > 0} {
        write "Skipping wait before collecting results ..."
    } else {
        write "Waiting $::test::postTxDelay seconds before collecting results ..."
        after [expr {$::test::postTxDelay * 1000}] {set done 1}
        vwait done
    }



    if {$::test::upstreamEnable} {
        ::IxOsLibrary::stopPacketGroups $::test::smtsPort
    }
    if {$::test::downstreamEnable} {
        ::IxOsLibrary::stopPacketGroups $::test::smPort
    }

    ## Collect statistics
    write "Collecting results ..."
    set header [format "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s"\
                "Direction"\
                "Stream Name"\
                "SVlan ID"\
                "CVlan ID"\
                "PCP(CVlan)"\
                "Tx Frames"\
                "Rx Frames"\
                "Rx Rate"\
                "Tput"\
                "Loss"\
                "Loss(scientific)"\
                "Rx MBits"]
    puts -nonewline $::TestLibrary::csv "$header"

    if {$::test::latencyMode eq "interArrivalJitter"} {
        set latHeader [format ",%s,%s,%s"\
                        "Avg Inter-Arrival (ms)"\
                        "Min Inter-Arrival (ms)"\
                        "Max Inter-Arrival (ms)"]
    } else {
        set latHeader [format ",%s,%s,%s"\
                        "Avg Latency (ms)"\
                        "Min Latency (ms)"\
                        "Max Latency (ms)"]
    }
    puts -nonewline $::TestLibrary::csv "$latHeader"

    set seqHeader [format ",%s,%s,%s,%s,%s,%s"\
                    "Small Sequence Error"\
                    "Big Sequence Error"\
                    "Reverse Sequence Error"\
                    "Total Sequence Error"\
                    "Sequence Gaps"\
                    "Duplicate Frames"]
    puts -nonewline $::TestLibrary::csv "$seqHeader"

    set diHeader [format ",%s,%s"\
                    "Data Integrity Frames"\
                    "Data Integrity Errors"]
    puts $::TestLibrary::csv "$diHeader"



    # Compute rate averages for each stream in the rateArray
    foreach name [array names ::test::rateArray] {
        set valueList $::test::rateArray($name)
        set mean [::math::statistics::mean $valueList]
        set ::test::rateArray($name) [expr {round($mean)}]
    }

    if {$::test::upstreamEnable} {
        foreach stream $upStreamList {
            getIntervalStats $stream 0 1
        }
    }

    if {$::test::downstreamEnable} {
        foreach stream $downStreamList {
            getIntervalStats $stream 0 1
        }
    }

    if {$::test::upstreamEnable} {
        write "Upstream Results:"
        getStreamStats $upStreamList upstream

        # No verification needed if we set it to -1
        if {$::test::ixOsUsTputThreshold != -1} {
            if {$tput_up >= $::test::ixOsUsTputThreshold} {
                lappend verification_calls [list IxOsUsTotalTput pass int >= $::test::ixOsUsTputThreshold $tput_up]
            } else {
                lappend verification_calls [list IxOsUsTotalTput fail int <  $::test::ixOsUsTputThreshold $tput_up]
            }
        }
    }


    # No verification if we set it to -1
    if {$::test::downstreamEnable} {
        write "Downstream Results:"
        getStreamStats $downStreamList downstream
        if {$::test::ixOsDsTputThreshold != -1} {
            if {$tput_down >= $::test::ixOsDsTputThreshold} {
                lappend verification_calls [list IxOsDsTotalTput pass int >= $::test::ixOsDsTputThreshold $tput_down]
            } else {
                lappend verification_calls [list IxOsDsTotalTput fail int <  $::test::ixOsDsTputThreshold $tput_down]
            }
        }
    }
    disconnect
    close $rateHandle

    foreach call $verification_calls {
        ::TestLibrary::saveCriteriaResult {*}$call
    }



    set plots [list]
    if {$::test::upstreamEnable} {
        set upstreamFile "$::TestLibrary::resDir/upstream_${::TestLibrary::runId}_${::TestLibrary::stepId}.csv"
        set usfid [open $upstreamFile w]
        puts $usfid $header
        close $usfid
        set cmd "grep up $rateFileName > $upstreamFile"
        if {[catch {exec {*}$cmd} err]} {
            write "Error generating upstream file: --$err--" error
            write "Error information: --$::errorCode--" error
        } else {
            #lappend plots [list 5 8 {0 1 2} $upstreamFile "RL Traffic" "Time (s)" "BPS" "Return Link"]
            lappend plots [list 6 9 {1 2 3} $upstreamFile "RL Traffic" "Time (s)" "BPS" "Return Link"]
        }
    }

    if {$::test::downstreamEnable} {
        set downstreamFile "$::TestLibrary::resDir/downstream_${::TestLibrary::runId}_${::TestLibrary::stepId}.csv"
        set dsfid [open $downstreamFile w]
        puts $dsfid $header
        close $dsfid
        set cmd "grep down $rateFileName >> $downstreamFile"
        if {[catch {exec {*}$cmd} err]} {
            write "Error generating downstream file: --$err--" error
            write "Error information: --$::errorCode--" error
        } else {
            #lappend plots [list 5 8 {0 1 2} $downstreamFile "FL Traffic" "Time (s)" "BPS" "Foward Link"]
            lappend plots [list 6 9 {1 2 3} $downstreamFile "FL Traffic" "Time (s)" "BPS" "Foward Link"]
        }
    }

    if {$plots ne [list]} {
        write "Creating the vipcat file..."
        ::TestLibrary::vipcatWriteFile $plots
    }
}



############
# Run Test #
############

::TestLibrary::start
run
::TestLibrary::end
