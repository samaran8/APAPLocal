* @ValidationCode : MjotMTU5Nzc3MjU4NDpDcDEyNTI6MTY4MjY3MzY4NTYxODpJVFNTOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 28 Apr 2023 14:51:25
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.E.MB.TXN.GET.ENTRIES(ENT.LIST)
*-------------------------------------------------------
*Description: This Nofile enquiry is to return the entires related to TFS transaction
*-------------------------------------------------------
* Input  Arg: Common Varaibles
* Output Arg: ENT.LIST -> List of entries
* Deals With: ENQUIRY>TXN.ENTRY.MB & ENQUIRY>REDO.TXN.ENTRY.MB
*-------------------------------------------------------
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
* 28-APRIL-2023      Conversion Tool       R22 Auto Conversion - VM to @VM 
* 28-APRIL-2023      Harsha                R22 Manual Conversion - No changes
*------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.T24.FUND.SERVICES



    ENT.LIST = ''
    GOSUB OPEN.FILES
    GOSUB PROCESS
RETURN
*-------------------------------------------------------
OPEN.FILES:
*-------------------------------------------------------
    FN.TFS = 'F.T24.FUND.SERVICES'
    F.TFS  = ''
    CALL OPF(FN.TFS,F.TFS)

    FN.TFS$NAU = 'F.T24.FUND.SERVICES$NAU'
    F.TFS$NAU = ''
    CALL OPF(FN.TFS$NAU,F.TFS$NAU)

RETURN
*-------------------------------------------------------
PROCESS:
*-------------------------------------------------------


    LOCATE 'TRANSACTION.REF' IN D.FIELDS<1> SETTING ID.POS THEN
        Y.TRANS.REF = D.RANGE.AND.VALUE<ID.POS>
    END
    IF Y.TRANS.REF[1,5] EQ 'T24FS' THEN
        GOSUB CHECK.TFS
        IF R.TFS THEN
            GOSUB REMAIN.PROCESS
        END
    END ELSE
        CALL E.MB.TXN.GET.ENTRIES(ENT.LIST)
    END

RETURN
*-------------------------------------------------------
CHECK.TFS:
*-------------------------------------------------------
    R.TFS = ''
    CALL F.READ(FN.TFS,Y.TRANS.REF,R.TFS,F.TFS,TFS.ERR)
    IF R.TFS ELSE
        CALL F.READ(FN.TFS$NAU,Y.TRANS.REF,R.TFS,F.TFS$NAU,TFS.ERR)
    END

RETURN
*-------------------------------------------------------
REMAIN.PROCESS:
*-------------------------------------------------------
    Y.UNDERLYING.ID = R.TFS<TFS.UNDERLYING>
    Y.ID.CNT = DCOUNT(Y.UNDERLYING.ID,@VM)
    Y.VAR1 = 1
    LOOP
    WHILE Y.VAR1 LE Y.ID.CNT
        Y.TXN.ID = Y.UNDERLYING.ID<1,Y.VAR1>
        D.RANGE.AND.VALUE<ID.POS> = Y.TXN.ID
        CALL E.MB.TXN.GET.ENTRIES(OUT.ARRAY)
        ENT.LIST<-1> = OUT.ARRAY
        Y.VAR1++
    REPEAT

RETURN
END
