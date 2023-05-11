* @ValidationCode : MjoxOTM2OTI1MjIzOkNwMTI1MjoxNjgzMDMxMTQ3MTQ1OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjJfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 02 May 2023 18:09:07
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R22_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.VAL.L.MG.ACT.NO
*-----------------------------------------------------------------------------
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.APAP.VAL.L.MG.ACT.NO
*------------------------------------------------------------------------------
*Description  : REDO.APAP.VAL.L.MG.ACT.NO is a validation routine for the version AZ.ACCOUNT,
*               OPEN.CPH which restricts the user from entering values into field mortgage account
*               no if the category code is not of CPH. Also alerts the user to link loan if
*               CPH category is used and no loan is linked
*Linked With  : AZ.ACCOUNT,OPEN.CPH
*In Parameter : N/A
*Out Parameter: N/A
*Linked File  : REDO.APAP.CPH.PARAMETER
*-------------------------------------------------------------------------------
* Modification History :
*-----------------------
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 29-07-2010       JEEVA T           ODR-2009-10-0346 B.21       Initial Creation
* 21-07-2011       JEEVAT               PACS00038165
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*19-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  FM to @FM , VM to @VM ,++ to +=
*19-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------
*--------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.REDO.APAP.CPH.PARAMETER
    $INSERT I_F.REDO.APAP.MORTGAGES.DETAIL
*--------------------------------------------------------------------------------
**********
MAIN.PARA:
**********

    GOSUB PROCESS.PARA
RETURN

*---------------------------------------------------------------------------------------------------------------------------------
**********
PROCESS.PARA:
**********
    FN.REDO.APAP.CPH.PARAMETER = 'F.REDO.APAP.CPH.PARAMETER'
    F.REDO.APAP.CPH.PARAMETER =''
    CALL OPF(FN.REDO.APAP.CPH.PARAMETER,F.REDO.APAP.CPH.PARAMETER)

    FN.REDO.APAP.MORTGAGES.DETAIL = 'F.REDO.APAP.MORTGAGES.DETAIL'
    F.REDO.APAP.MORTGAGES.DETAIL = ''
    CALL OPF(FN.REDO.APAP.MORTGAGES.DETAIL,F.REDO.APAP.MORTGAGES.DETAIL)

    Y.MG.ACT.NOS=COMI
    GOSUB READ.CPH.PARAMETER
    Y.DEP.CAT=R.NEW(AZ.CATEGORY)
    GOSUB VAL.CATEGORY.AND.LOAN
RETURN
*---------------------------------------------------------------------------------------------------------------------------------
*******************
READ.CPH.PARAMETER:
*******************
    CALL CACHE.READ('F.REDO.APAP.CPH.PARAMETER','SYSTEM',R.REDO.APAP.CPH.PARAMETER,Y.REDO.APAP.CPH.PARAMETER.ERR)

*********************PACS00038165 starts            ******************
    CALL F.READ(FN.REDO.APAP.MORTGAGES.DETAIL,Y.MG.ACT.NOS,R.REDO.APAP.MORTGAGES.DETAIL,F.REDO.APAP.MORTGAGES.DETAIL,Y.MG.ERR)
*********************PACS00038165 ends**************
    Y.CPH.CATS=R.REDO.APAP.CPH.PARAMETER<CPH.PARAM.CPH.CATEGORY>
RETURN
*---------------------------------------------------------------------------------------------------------------------------------
***********************
VAL.CATEGORY.AND.LOAN:
***********************
    CHANGE @VM TO @FM IN Y.CPH.CATS
    LOCATE Y.DEP.CAT IN Y.CPH.CATS SETTING Y.LOC.L.MG.ACT.NO.POS THEN
        IF  NOT(Y.MG.ACT.NOS) THEN
            ETEXT = 'EB-REDO.NO.LOAN.LINKED'
            CALL STORE.END.ERROR
        END
    END ELSE
        IF Y.MG.ACT.NOS THEN
            ETEXT = 'EB-REDO.NOT.CPH.CAT'
            CALL STORE.END.ERROR
            RETURN
        END
    END
    IF R.REDO.APAP.MORTGAGES.DETAIL THEN
        Y.STAT = R.REDO.APAP.MORTGAGES.DETAIL<MG.DET.STATUS>
        Y.CNT = DCOUNT(Y.STAT,@VM)
        Y.COUNT =1
        CHANGE @VM TO @FM IN Y.STAT
        LOOP
        WHILE Y.COUNT LE Y.CNT
            LOCATE Y.STAT<Y.COUNT> IN R.REDO.APAP.CPH.PARAMETER<CPH.PARAM.ALLOWED.STATUS,1> SETTING POS.MG THEN
                ETEXT = 'EB-REDO.NO.LOAN.LINKED'
                CALL STORE.END.ERROR
                RETURN
            END
            Y.COUNT += 1 ;*R22 AUTO CODE CONVERSION
        REPEAT
    END
*CALL APAP.REDOAPAP.REDO.APAP.VAL.FHA.DETS
    CALL APAP.REDOAPAP.redoApapValFhaDets()
RETURN
*---------------------------------------------------------------------------------------------------------------------------------
END
