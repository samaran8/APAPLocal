* @ValidationCode : Mjo2NjA5ODc1NzpDcDEyNTI6MTY4NDMzMzcxNDI1NTpzYW1hcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 17 May 2023 19:58:34
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
$PACKAGE APAP.TAM
SUBROUTINE REDO.UPD.INT.AMT.OTHR
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: H GANESH
* PROGRAM NAME: REDO.UPD.INT.AMT.OTHR
* ODR NO      : PACS00045355-B.16
*----------------------------------------------------------------------
*DESCRIPTION: This is a post routine for other activity for other(like payment schedule,term amount principal int) property
* to update the interest amounts in local fields.


*IN PARAMETER  : NA
*OUT PARAMETER : NA
*LINKED WITH   : NA
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*31-MAR-2011  H GANESH     PACS00045355-B.16  INITIAL CREATION
** 18-04-2023 R22 Auto Conversion - FM TO @FM, VM to @VM, SM to @SM
** 18-04-2023 Skanda R22 Manual Conversion - added APAP.TAM ,CALL RTN FORMAT MODIFIED
*----------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.ACCOUNT.DETAILS
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_F.AA.INTEREST
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.PAYMENT.SCHEDULE
    $INSERT I_F.REDO.OFS.PARAM
    $USING APAP.AA


    IF c_aalocActivityStatus NE 'AUTH' AND c_aalocActivityStatus NE 'AUTH-REV' THEN         ;* Routine exits if other than authorisation stage.
        RETURN
    END

    GOSUB OPEN.FILES
    GOSUB GET.MULTI.LOC.REF
    GOSUB PROCESS

RETURN
*----------------------------------------------------------------------
OPEN.FILES:
*----------------------------------------------------------------------

    INT.AMT=0

RETURN
*----------------------------------------------------------------------
GET.MULTI.LOC.REF:
*----------------------------------------------------------------------
    LOC.REF.APPLICATION="AA.PRD.DES.INTEREST"
    LOC.REF.FIELDS="L.AA.INT.AMTOLD":@VM:"L.AA.INT.AMTNEW":@VM:"L.AA.DIFF.AMT"
    LOC.REF.POS=''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
    POS.L.AA.INT.AMTOLD=LOC.REF.POS<1,1>
    POS.L.AA.INT.AMTNEW=LOC.REF.POS<1,2>
    POS.L.AA.DIFF.AMT=  LOC.REF.POS<1,3>

RETURN
*----------------------------------------------------------------------
PROCESS:
*----------------------------------------------------------------------


    Y.ARRANGEMENT.ID=c_aalocArrId
    GOSUB GET.INTEREST.PROP

*CALL APAP.TAM.REDO.GET.TOTAL.INTEREST(Y.ARRANGEMENT.ID,PROPERTY,INT.AMT) ;* R22 Manual conversion
    CALL APAP.TAM.RedoGetTotalInterest(Y.ARRANGEMENT.ID,PROPERTY,INT.AMT) ;* R22 Manual conversion


    IF c_aalocPropClassId EQ 'INTEREST' THEN
        IF R.NEW(AA.INT.LOCAL.REF)<1,POS.L.AA.INT.AMTNEW> NE INT.AMT THEN
            R.NEW(AA.INT.LOCAL.REF)<1,POS.L.AA.INT.AMTOLD>=R.NEW(AA.INT.LOCAL.REF)<1,POS.L.AA.INT.AMTNEW>
            R.NEW(AA.INT.LOCAL.REF)<1,POS.L.AA.INT.AMTNEW>=INT.AMT
            R.NEW(AA.INT.LOCAL.REF)<1,POS.L.AA.DIFF.AMT>= R.NEW(AA.INT.LOCAL.REF)<1,POS.L.AA.INT.AMTOLD>-R.NEW(AA.INT.LOCAL.REF)<1,POS.L.AA.INT.AMTNEW>     ;* Both Old and New amounts are same, so obviously zero
        END
    END ELSE
*CALL REDO.CRR.GET.CONDITIONS(Y.ARRANGEMENT.ID,'','INTEREST',PROPERTY,R.INT.COND,RET.ERR) ;*R22 MANUAL CODE CONVERSION
        CALL APAP.AA.redoCrrGetConditions(Y.ARRANGEMENT.ID,'','INTEREST',PROPERTY,R.INT.COND,RET.ERR) ;*R22 MANUAL CODE CONVERSION
        Y.GET.NEW.AMT=R.INT.COND<AA.INT.LOCAL.REF,POS.L.AA.INT.AMTNEW>
        IF Y.GET.NEW.AMT NE INT.AMT THEN
*GOSUB POST.OFS
        END
    END

RETURN
*---------------------------------------------------------------------------
GET.INTEREST.PROP:
*---------------------------------------------------------------------------
* This part gets the principal interest property of that arrangement.

    PROP.NAME='PRINCIPAL'       ;* Interest Property to obtain
    OUT.PROP=''
*CALL REDO.GET.INTEREST.PROPERTY(Y.ARRANGEMENT.ID,PROP.NAME,OUT.PROP,ERR) ;*R22 MANUAL CODE CONVERSION
    CALL APAP.TAM.redoGetInterestProperty(Y.ARRANGEMENT.ID,PROP.NAME,OUT.PROP,ERR) ;*R22 MANUAL CODE CONVERSION
    PROPERTY=OUT.PROP
RETURN

*---------------------------------------------------------------------------
POST.OFS:
*---------------------------------------------------------------------------
    R.INT.CONDITION = ''
    R.INT.CONDITION<AA.INT.LOCAL.REF,POS.L.AA.INT.AMTOLD>=R.INT.COND<AA.INT.LOCAL.REF,POS.L.AA.INT.AMTNEW>
    R.INT.CONDITION<AA.INT.LOCAL.REF,POS.L.AA.INT.AMTNEW>=INT.AMT
    R.INT.CONDITION<AA.INT.LOCAL.REF,POS.L.AA.DIFF.AMT>=R.INT.COND<AA.INT.LOCAL.REF,POS.L.AA.INT.AMTOLD>-R.INT.COND<AA.INT.LOCAL.REF,POS.L.AA.INT.AMTNEW>
    Y.PROP.ACTIVITY    = ''
    Y.PROP.ACTIVITY<1> = PROPERTY
    Y.PROP.ACTIVITY<2> = 'LENDING-CHANGE-':PROPERTY
*CALL REDO.AA.BUILD.OFS(c_aalocArrId,R.INT.CONDITION,Y.PROP.ACTIVITY,OFS.MSG) ;*R22 MANUAL CODE CONVERSION
    CALL APAP.AA.redoAaBuildOfs(c_aalocArrId,R.INT.CONDITION,Y.PROP.ACTIVITY,OFS.MSG) ;*R22 MANUAL CODE CONVERSION
    OFS.SRC = 'AA.INT.UPDATE'
    OPTIONS = ''
    OFS.STRING.FINAL = ''
    OFS.MSG.ID = ''
    OFS.STRING.FINAL:=OFS.MSG:"EFFECTIVE.DATE:1:1=":c_aalocActivityEffDate
    CALL OFS.POST.MESSAGE(OFS.STRING.FINAL,OFS.MSG.ID,OFS.SRC,OPTIONS)

RETURN
END
