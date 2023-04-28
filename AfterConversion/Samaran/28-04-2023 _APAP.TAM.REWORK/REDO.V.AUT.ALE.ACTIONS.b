* @ValidationCode : MjotMTkxNzAxNTQwNDpDcDEyNTI6MTY4MjY5MDEwNTk4NDpzYW1hcjotMTotMTowOjE6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 28 Apr 2023 19:25:05
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.V.AUT.ALE.ACTIONS
*--------------------------------------------------------------------------------------------------------
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.V.AUT.ALE.ACTIONS
*--------------------------------------------------------------------------------------------------------
*Description  : To Update concat table
*Linked With  : VERSION OF AC.LOCKED.EVENTS,MB, AC.LOCKED.EVENTS,OFS
*In Parameter : N/A
*Out Parameter: N/A
*--------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 23 Nov 2010    Mohammed Anies K      ODR-2010-09-0251       Initial Creation
* 3 Nov 2011     Kavitha               PACS00055620            PACS00055620 fix
*--------------------------------------------------------------------------------------------------------
*Modification History
*DATE                WHO                         REFERENCE                DESCRIPTION
*19-04-2023       Conversion Tool        R22 Auto Code conversion          No Changes
*19-04-2023       Samaran T               R22 Manual Code Conversion       FM TO @FM
*---------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AC.LOCKED.EVENTS
    $INSERT I_F.T24.FUND.SERVICES
    $INSERT I_F.REDO.INTRANSIT.LOCK

    FN.TFS = 'F.T24.FUND.SERVICES'
    F.TFS = ''
    CALL OPF(FN.TFS,F.TFS)

    FN.REDO.TRANSIT.LOCK = 'F.REDO.INTRANSIT.LOCK'
    F.REDO.TRANSIT.LOCK = ''
    CALL OPF(FN.REDO.TRANSIT.LOCK,F.REDO.TRANSIT.LOCK)

    GOSUB PROCESS

RETURN
*-------------------
PROCESS:
*PACS00055620 -s
    R.REDO.TRANSIT.LOCK = ''



    TFS.ID = R.NEW(AC.LCK.DESCRIPTION)
    ACCT.ID = R.NEW(AC.LCK.ACCOUNT.NUMBER)

    CALL F.READ(FN.REDO.TRANSIT.LOCK,ACCT.ID,R.REDO.TRANSIT.LOCK,F.REDO.TRANSIT.LOCK,ERR)
*PACS00055620 -e

    IF V$FUNCTION EQ "I" THEN

        LOCATE ID.NEW IN R.REDO.TRANSIT.LOCK SETTING LOCK.POS ELSE
            R.REDO.TRANSIT.LOCK<-1> = ID.NEW
            CALL F.WRITE(FN.REDO.TRANSIT.LOCK,ACCT.ID,R.REDO.TRANSIT.LOCK)
        END

    END

    IF V$FUNCTION EQ "R" THEN

        LOCATE ID.NEW IN R.REDO.TRANSIT.LOCK SETTING LOCK.POS THEN
            DEL R.REDO.TRANSIT.LOCK<LOCK.POS>
        END

        TOTAL.CNTR = DCOUNT(R.REDO.TRANSIT.LOCK,@FM) ;*R22 MANUAL CODE CONVERSION
        IF TOTAL.CNTR GT 0 THEN
            CALL F.WRITE(FN.REDO.TRANSIT.LOCK,ACCT.ID,R.REDO.TRANSIT.LOCK)
        END ELSE
            CALL F.DELETE(FN.REDO.TRANSIT.LOCK,ACCT.ID)
        END

    END

RETURN
*--------------------------------------------------------------------------
END
