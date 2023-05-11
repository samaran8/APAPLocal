* @ValidationCode : Mjo5MjY4MDc3MDA6Q3AxMjUyOjE2ODEyODc0MDI0MjY6c2FtYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 13:46:42
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.INP.TELLER.CASH.DENOM
*--------------------------------------------------------------------------------------------------------
* Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By      : Temenos Application Management
* Program   Name    : REDO.V.INP.TELLER.CASH.DENOM
*--------------------------------------------------------------------------------------------------------
* Description       : This is an INPUT routine, the routine validates if the amount and the denominations
*                     amount is equal or not and throw an override accordingly
* Linked With       : Versions TELLER.ID,REDO.TELLER.CLOSE
* In  Parameter     : N/A
* Out Parameter     : N/A
* Files  Used       : TELLER                              As          I       Mode
*--------------------------------------------------------------------------------------------------------
* Modification Details:
* =====================
* Date          Who                 Reference             Description
* ------        -----               -------------         -------------
* 30 Aug 2011   Ganesh R            Version Integration   Initial Creation
* 05/06/2013    VIGNESH KUMAAR R    PACS00273071          Revamp of the routine with DENOM validation
* 15/07/2013    Vignesh Kumaar R    PACS00306797          Remove EB-REDO.DENOM.MISMATCH Error
* 08/04/2014    Vignesh Kumaar R    PACS00352671          DIFFERENCE IN DENOMINATION OVERRIDE
* 19/05/2015    Vignesh Kumaar R    PACS00458844          COMPLETE REVAMP OF THE ROUTINE
*--------------------------------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*12-04-2023            Conversion Tool             R22 Auto Code conversion                       VM TO @VM,++ to +=1
*12-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER.ID

    GET.CCY = R.NEW(TT.TID.CURRENCY)
    GET.DIF = R.NEW(TT.TID.DIFFERENCE)
    GET.BAL = R.NEW(TT.TID.TILL.BALANCE)
    GET.CLS = R.NEW(TT.TID.TILL.CLOS.BAL)

    GET.CNT = DCOUNT(GET.CCY,@VM)
    GET.POS = 1

    LOOP
    WHILE GET.POS LE GET.CNT

        IF GET.CLS<1,GET.POS> AND (GET.BAL<1,GET.POS> EQ 0 OR GET.BAL<1,GET.POS> EQ '') THEN
            ETEXT = "TT-DENOMINATION.MISS"
            CALL STORE.END.ERROR
            RETURN
        END

        IF GET.DIF<1,GET.POS> NE 0 THEN
            TEXT = "REDO.DENOM.DIFFERENT"
            CURR.NO = DCOUNT(R.NEW(TT.TID.OVERRIDE),@VM) + 1
            CALL STORE.OVERRIDE(CURR.NO)
        END
        GET.POS += 1
    REPEAT
RETURN

*----------------------------------------------------------------------------------------------------------------------
END
