* @ValidationCode : MjotMTA3NzM5ODY3MzpDcDEyNTI6MTY4MDc1OTY4NDUzMTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 06 Apr 2023 11:11:24
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.EXPIRE.GVT.NGVT(CHQ.DETAILS.ID)
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : SUDHARSANAN S
* Program Name  : REDO.B.EXPIRE.GVT.NGVT
*-------------------------------------------------------------------------

* Description :This is batch routine attached to stage "D990" for Expiration of
* Administration cheque Non Government and Government
* In parameter : None
* out parameter : None
*------------------------------------------------------------------------
*----------------------------------------------------------------------------
* Revision History:
* -----------------
* Date          Name                    Reference            Description
* -------       ----                    ---------            ------------
* 01-06-2011  Bharath G                 PACS00072408         Date value added
* 30-04-2012  Sudharsanan S             PACS00188192         Check Condition Added
* 04-APR-2023     Conversion tool    R22 Auto conversion    TNO to C$T24.SESSION, CONVERT to CHANGE
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_F.USER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.CATEG.ENTRY
    $INSERT I_F.REDO.ADMIN.CHQ.PARAM
    $INSERT I_F.REDO.ADMIN.CHQ.DETAILS
    $INSERT I_REDO.B.EXPIRE.GVT.NGVT.COMMON

    GOSUB PROCESS
RETURN
************
PROCESS:
************
    EB.ACCT.ERR.MSG = ''
    R.CHQ.DETAILS=''
    CALL F.READ(FN.REDO.ADMIN.CHQ.DETAILS,CHQ.DETAILS.ID,R.CHQ.DETAILS,F.REDO.ADMIN.CHQ.DETAILS,CHQ.ERR)
    IF  R.CHQ.DETAILS<ADMIN.CHQ.DET.CHEQUE.INT.ACCT> EQ NON.GVMNT.ACCT THEN
        R.CHQ.DETAILS<ADMIN.CHQ.DET.STATUS>='RECLASSIFY'
        R.CHQ.DETAILS<ADMIN.CHQ.DET.RECLASSIFY.DATE> = TODAY    ;* PACS00072408
    END ELSE
        R.CHQ.DETAILS<ADMIN.CHQ.DET.STATUS>='REISSUED'
        R.CHQ.DETAILS<ADMIN.CHQ.DET.RECLASSIFY.DATE> = TODAY    ;* PACS00072408
    END
    Y.CURR.NO=R.CHQ.DETAILS<ADMIN.CHQ.DET.CURR.NO>
    R.CHQ.DETAILS<ADMIN.CHQ.DET.CURR.NO>=Y.CURR.NO+1
    R.CHQ.DETAILS<ADMIN.CHQ.DET.INPUTTER>=C$T24.SESSION.NO:'_':OPERATOR
    TEMPTIME = OCONV(TIME(),"MTS")
    TEMPTIME = TEMPTIME[1,5]
    CHANGE ':' TO '' IN TEMPTIME
    CHECK.DATE = DATE()
    R.CHQ.DETAILS<ADMIN.CHQ.DET.DATE.TIME>= OCONV(CHECK.DATE,"DY2"):FMT(OCONV(CHECK.DATE,"DM"),"R%2"):OCONV(CHECK.DATE,"DD"):TEMPTIME
    R.CHQ.DETAILS<ADMIN.CHQ.DET.CO.CODE> = ID.COMPANY
    R.CHQ.DETAILS<ADMIN.CHQ.DET.AUTHORISER>=C$T24.SESSION.NO:'_':OPERATOR
    R.CHQ.DETAILS<ADMIN.CHQ.DET.DEPT.CODE> =R.USER<EB.USE.DEPARTMENT.CODE>
    CALL F.WRITE(FN.REDO.ADMIN.CHQ.DETAILS,CHQ.DETAILS.ID,R.CHQ.DETAILS)
    GOSUB ACCT.ENTRIES
RETURN
**************
ACCT.ENTRIES:
**************
*Raising Debit Entry
    Y.ACCOUNT.NO=R.CHQ.DETAILS<ADMIN.CHQ.DET.CHEQUE.INT.ACCT>
    Y.AMOUNT=R.CHQ.DETAILS<ADMIN.CHQ.DET.AMOUNT>
    Y.ID = CHQ.DETAILS.ID
    Y.ISSUE.ACCOUNT=R.CHQ.DETAILS<ADMIN.CHQ.DET.ISSUE.ACCOUNT>
    IF Y.ACCOUNT.NO AND Y.AMOUNT THEN     ;*****PACS00188192
        GOSUB UPDATE.ACCT.ENTRIES
    END
RETURN
*********************
UPDATE.ACCT.ENTRIES:
*********************
    CALL F.READ(FN.ACCOUNT,Y.ISSUE.ACCOUNT,R.ACCOUNT,F.ACCOUNT,ACC.ERR)
*    Y.CUSTOMER=R.ACCOUNT<AC.CUSTOMER>
    Y.ACCT.OFFICER=R.ACCOUNT<AC.ACCOUNT.OFFICER>
    Y.CATEGORY=R.ACCOUNT<AC.CATEGORY>
    Y.CURR=R.ACCOUNT<AC.CURRENCY>
    R.STMT.ARR = ''
    R.STMT.ARR<AC.STE.ACCOUNT.NUMBER> =  Y.ACCOUNT.NO
    R.STMT.ARR<AC.STE.COMPANY.CODE> = ID.COMPANY
    R.STMT.ARR<AC.STE.AMOUNT.LCY> = -1*Y.AMOUNT
    R.STMT.ARR<AC.STE.TRANSACTION.CODE> = 213
    R.STMT.ARR<AC.STE.THEIR.REFERENCE> = CHQ.DETAILS.ID
*    R.STMT.ARR<AC.STE.CUSTOMER.ID> = Y.CUSTOMER
    R.STMT.ARR<AC.STE.ACCOUNT.OFFICER> = Y.ACCT.OFFICER
    R.STMT.ARR<AC.STE.PRODUCT.CATEGORY> = Y.CATEGORY
    R.STMT.ARR<AC.STE.VALUE.DATE> = TODAY
    R.STMT.ARR<AC.STE.CURRENCY> = Y.CURR
    R.STMT.ARR<AC.STE.OUR.REFERENCE> = CHQ.DETAILS.ID
    R.STMT.ARR<AC.STE.TRANS.REFERENCE>= CHQ.DETAILS.ID
    R.STMT.ARR<AC.STE.SYSTEM.ID> = "CHQ"
    R.STMT.ARR<AC.STE.BOOKING.DATE> = LAST.WORK.DATE
    R.STMT.ARR<AC.STE.CURRENCY.MARKET> = "1"
    R.STMT.ARR<AC.STE.EXPOSURE.DATE> = TODAY
    R.STMT.ARR<AC.STE.DEPARTMENT.CODE> = R.USER<EB.USE.DEPARTMENT.CODE>

    MULTI.STMT=''
    MULTI.STMT<-1> = LOWER(R.STMT.ARR)
    GOSUB RAISE.CR.ENTRIES
    ID.NEW = 1
    V = ADMIN.CHQ.DET.AUDIT.DATE.TIME
    CALL EB.ACCOUNTING.WRAPPER("CHQ","SAO",MULTI.STMT,'',EB.ACCT.ERR.MSG)
RETURN
*-------------------------------------------------------------------------
RAISE.CR.ENTRIES:
*~~~~~~~~~~~~~~~~~~
*Raising Credit entry
    R.STMT.ARR = ''
    LOCATE  Y.ACCOUNT.NO IN R.CHQ.PARAM<ADMIN.CHQ.PARAM.ACCOUNT,1> SETTING POS THEN
        Y.ACCT.NO = R.CHQ.PARAM<ADMIN.CHQ.PARAM.UNPAID.ADMIN,POS>
    END
    IF Y.ACCT.NO THEN
        R.STMT.ARR<AC.STE.ACCOUNT.NUMBER> =  Y.ACCT.NO
        R.STMT.ARR<AC.STE.COMPANY.CODE> = ID.COMPANY
        R.STMT.ARR<AC.STE.AMOUNT.LCY> = Y.AMOUNT
        R.STMT.ARR<AC.STE.TRANSACTION.CODE> = 213
        R.STMT.ARR<AC.STE.THEIR.REFERENCE> = CHQ.DETAILS.ID
*    R.STMT.ARR<AC.STE.CUSTOMER.ID> = Y.CUSTOMER
        R.STMT.ARR<AC.STE.ACCOUNT.OFFICER> = Y.ACCT.OFFICER
        R.STMT.ARR<AC.STE.PRODUCT.CATEGORY> = Y.CATEGORY
        R.STMT.ARR<AC.STE.VALUE.DATE> = TODAY
        R.STMT.ARR<AC.STE.CURRENCY> = Y.CURR
        R.STMT.ARR<AC.STE.OUR.REFERENCE> = CHQ.DETAILS.ID
        R.STMT.ARR<AC.STE.TRANS.REFERENCE>= CHQ.DETAILS.ID
        R.STMT.ARR<AC.STE.SYSTEM.ID> = "CHQ"
        R.STMT.ARR<AC.STE.BOOKING.DATE> = LAST.WORK.DATE
        R.STMT.ARR<AC.STE.CURRENCY.MARKET> = "1"
        R.STMT.ARR<AC.STE.EXPOSURE.DATE> = TODAY
        R.STMT.ARR<AC.STE.DEPARTMENT.CODE> = R.USER<EB.USE.DEPARTMENT.CODE>
        MULTI.STMT<-1> = LOWER(R.STMT.ARR)
    END
RETURN
**************************************************************************************************
END
