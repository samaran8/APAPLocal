* @ValidationCode : MjoxOTUxMjE3NDI4OkNwMTI1MjoxNjgwNjg4MDAzNzkzOklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 15:16:43
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.CR.CAMP.NOT.VALID
*-------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This routine is attached to CR.OPPORTUNITY, CR.CONTACT.LOG to check if the campaign in valid

* INPUT/OUTPUT:
*--------------
* IN  : R.DATA
* OUT : L.CUST.ID
*-------------------------------------------------------------------------
*   Date               who           Reference            Description
* 13-SEP-2011     SHANKAR RAJU     ODR-2011-07-0162      Initial Creation
* 05.04.2023       Conversion Tool       R22            Auto Conversion     - F TO CACHE
* 05.04.2023       Shanmugapriya M       R22            Manual Conversion   - No changes
*
*-------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_PW.COMMON
    $INSERT I_F.PW.PROCESS
    $INSERT I_F.CR.OPPORTUNITY
    $INSERT I_F.CR.CONTACT.LOG
    $INSERT I_F.CR.CAMPAIGN.GENERATOR
    $INSERT I_F.CR.OPPORTUNITY.GENERATOR

    GOSUB INITIALISE
    GOSUB PROCESS

RETURN

INITIALISE:
*----------

    FN.PW.PROCESS = 'F.PW.PROCESS'
    F.PW.PROCESS  = ''
    R.PW.PROCESS  = ''
    CALL OPF(FN.PW.PROCESS,F.PW.PROCESS)

    FN.PW.ACTIVITY.TXN = 'F.PW.ACTIVITY.TXN'
    F.PW.ACTIVITY.TXN = ''
    CALL OPF(FN.PW.ACTIVITY.TXN,F.PW.ACTIVITY.TXN)

    FN.CR.OPPORTUNITY = 'F.CR.OPPORTUNITY'
    F.CR.OPPORTUNITY = ''
    CALL OPF(FN.CR.OPPORTUNITY,F.CR.OPPORTUNITY)

    FN.CR.CAMPAIGN.GENERATOR = 'F.CR.CAMPAIGN.GENERATOR'
    F.CR.CAMPAIGN.GENERATOR  = ''
    R.CR.CAMPAIGN.GENERATOR  = ''
    CALL OPF(FN.CR.CAMPAIGN.GENERATOR,F.CR.CAMPAIGN.GENERATOR)

    FN.CR.OPPORTUNITY.GENERATOR = 'F.CR.OPPORTUNITY.GENERATOR'
    F.CR.OPPORTUNITY.GENERATOR  = ''
    R.CR.OPPORTUNITY.GENERATOR  = ''
    CALL OPF(FN.CR.OPPORTUNITY.GENERATOR,F.CR.OPPORTUNITY.GENERATOR)

RETURN

PROCESS:
*-------

    IF APPLICATION EQ 'CR.OPPORTUNITY' THEN
        IF R.NEW(CR.OP.END.DATE) LT TODAY THEN
            AF = CR.OP.END.DATE
            ETEXT = "EB-REDO.CAMP.INVALID"
            CALL STORE.END.ERROR
        END
    END ELSE
        PROCESS.TXN.ID = PW$ACTIVITY.TXN.ID
        CALL PW.FIND.PROCESS(PROCESS.TXN.ID,PW.PROCESS.ID)      ;* get the PW.PROCESS name
        CALL F.READ(FN.PW.PROCESS,PW.PROCESS.ID,R.PW.PROCESS,F.PW.PROCESS,ERR.PW.PROCESS)

        Y.CAMP.ID = R.PW.PROCESS<PW.PROC.CAMP.GEN.ID>
        Y.OPP.ID  = R.PW.PROCESS<PW.PROC.OPP.GEN.ID>

        IF Y.CAMP.ID EQ '' THEN
            CALL F.READ(FN.CR.OPPORTUNITY.GENERATOR,Y.OPP.ID,R.CR.OPPORTUNITY.GENERATOR,F.CR.OPPORTUNITY.GENERATOR,ERR.OPP)
            GOSUB GET.END.DATE
            IF Y.END.DATE LT TODAY THEN
                AF = CR.CONT.LOG.CONTACT.STATUS
                ETEXT = "EB-REDO.CAMP.INVALID"
                CALL STORE.END.ERROR
            END
        END ELSE
            CALL CACHE.READ(FN.CR.CAMPAIGN.GENERATOR, Y.CAMP.ID, R.CR.CAMPAIGN.GENERATOR, ERR.CAMP)   ;** R22 Auto conversion - F TO CACHE
            Y.END.DATE = R.CR.CAMPAIGN.GENERATOR<CR.CG.CAMP.END.DATE>
            IF Y.END.DATE LT TODAY THEN
                AF = CR.CONT.LOG.CONTACT.STATUS
                ETEXT = "EB-REDO.CAMP.INVALID"
                CALL STORE.END.ERROR
            END
        END

    END
RETURN
*-----------------------------------------------------------------------------
GET.END.DATE:
*------------
* Get the end date which is based on start date and duration
    YDATE = R.CR.OPPORTUNITY.GENERATOR<CR.OG.START.DATE>
    DURATION = R.CR.OPPORTUNITY.GENERATOR<CR.OG.DURATION>
*
    GOSUB DURATION.CONVERSION

    CALL CDT('',YDATE,'+':DURATION.IN.DAYS)
*
    Y.END.DATE = YDATE

RETURN
*-----------------------------------------------------------------------------
DURATION.CONVERSION:
*-------------------
* Month,weeks to days
    BEGIN CASE
        CASE DURATION[1] EQ 'W'
            NO.OF.WEEKS = DURATION[1,LEN(DURATION)-1]
            DURATION.IN.DAYS = NO.OF.WEEKS * 7 :"C"
        CASE DURATION[1] EQ 'M'
            NO.OF.MONTHS = DURATION[1,LEN(DURATION)-1]
            DURATION.IN.DAYS = NO.OF.MONTHS * 30 :"C"
        CASE DURATION[1] EQ 'Y'
            NO.OF.YEARS = DURATION[1,LEN(DURATION)-1]
            DURATION.IN.DAYS = NO.OF.YEARS * 365 :"C"
        CASE DURATION[1] EQ 'D'
            NO.OF.DAYS = DURATION[1,LEN(DURATION)-1]
            DURATION.IN.DAYS = NO.OF.DAYS:"C"
    END CASE

RETURN
*-----------------------------------------------------------------------------
END
