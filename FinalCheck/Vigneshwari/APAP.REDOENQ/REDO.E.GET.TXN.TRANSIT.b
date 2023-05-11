$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.GET.TXN.TRANSIT(Y.FINAL.ARRAY)

*---------------------------------------------------------------------------------
*MODIFICATION:
*---------------------------------------------------------------------------------
* DATE           ODR                   DEVELOPER               VERSION
*--------       ----------------      -------------           --------------------
* 3.12.2010     ODR-2010-11-0211      Prabhu N                Initial creation
* 25/07/2015    PACS00468003          Vignesh Kumaar R        VISION PLUS - TRANSIT FUNDS
* 11-APRIL-2023      Conversion Tool       R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN, VM to @VM and -- to -=
* 11-APRIL-2023      Harsha                R22 Manual Conversion - No changes
*---------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_System
    $INSERT I_ENQUIRY.COMMON

    GOSUB INITIALISE
    GOSUB PROCESS
RETURN
*---------------------------------------------------------------------------------
INITIALISE:
*---------------------------------------------------------------------------------

    CREDIT.CARD.ID = System.getVariable('CURRENT.CARD.ID')
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN	  ;*R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN
        CREDIT.CARD.ID = ""
    END
    CREDIT.CARD.ID = FMT(CREDIT.CARD.ID, 'R%19')

RETURN

*---------------------------------------------------------------------------------
PROCESS:
*---------------------------------------------------------------------------------

    ACTIVATION = 'WS_T24_VPLUS'
    WS.DATA = ''
    WS.DATA<1> = 'CONSULTA_TRANSITO'
    WS.DATA<2> = CREDIT.CARD.ID

* Invoke VisionPlus Web Service
    CALL REDO.VP.WS.CONSUMER(ACTIVATION, WS.DATA)

* Credit Card exits - Info obtained OK
    IF WS.DATA<1> EQ 'OK' THEN

        GOSUB GET.TRANSIT.INFO
    END ELSE
* 'ERROR/OFFLINE'
        ENQ.ERROR<1> = WS.DATA<2>
    END

RETURN

GET.TRANSIT.INFO:
*----------------*

    WS.DATA = CHANGE(WS.DATA,'*',@VM)
    Y.CNT = DCOUNT(WS.DATA<7>,@VM)

    IF WS.DATA<7,Y.CNT> EQ '' THEN
        Y.CNT -= 1
    END

    LOOP
    WHILE Y.CNT GT 0
        IF WS.DATA<6,Y.CNT> EQ '1' THEN
            Y.CCY.MNE = 'RD$'
        END ELSE
            Y.CCY.MNE = 'US$'
        END

        IF WS.DATA<8,Y.CNT> EQ 'D' THEN
            Y.AMT = WS.DATA<7,Y.CNT>:'##':'0'
        END ELSE
            Y.AMT = '0':'##':WS.DATA<7,Y.CNT>
        END

        Y.FINAL.ARRAY<-1> = WS.DATA<3,Y.CNT>:'##':WS.DATA<4,Y.CNT>:'##':WS.DATA<5,Y.CNT>:'##':Y.CCY.MNE:'##':Y.AMT
        Y.CNT -= 1
    REPEAT

RETURN
