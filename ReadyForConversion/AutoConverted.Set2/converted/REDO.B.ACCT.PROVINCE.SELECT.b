SUBROUTINE REDO.B.ACCT.PROVINCE.SELECT
*-------------------------------------------------------------------------------
* Company Name      : PAGE SOLUTIONS, INDIA
* Developed By      :
* Reference         :
*-------------------------------------------------------------------------------
* Subroutine Type   : B
* Attached to       :
* Attached as       : Multi threaded Batch Routine.
*-------------------------------------------------------------------------------
* Input / Output :
*----------------
* IN     :
* OUT    :
*-------------------------------------------------------------------------------
* Description: This is a .SELECT Subroutine.
*
*-------------------------------------------------------------------------------
* Modification History
* Defect Reference       Modified By                    Date of Change        Change Details
*
*-----------------------------------------------------------------------------------------------------------------
*                       Ashokkumar.V.P                  17/02/2016           Changes to avoid the customer data issue
*-------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COMPANY
    $INSERT I_REDO.B.ACCT.PROVINCE.COMMON

    GOSUB INIT
    GOSUB PROCESS.PARA
RETURN
*-------------------------------------------------------------------------------
PROCESS.PARA:
*------------
    CALL EB.CLEAR.FILE(FN.DR.REG.PROV.WORKFILE, F.DR.REG.PROV.WORKFILE)

    SEL.COMP = "SSELECT ":FN.COMPANY
    CALL EB.READLIST(SEL.COMP,SEL.CLIST,'',REC.CLST,SEL.ERR)

    LOOP
        REMOVE COMP.ID FROM SEL.CLIST SETTING CMP.POSN
    WHILE COMP.ID:CMP.POSN
        YMNEMONIC = ''; ERR.COMP = ''; R.COMPANY1 = ''
        CALL CACHE.READ(FN.COMPANY,COMP.ID,R.COMPANY1,ERR.COMP)
        YMNEMONIC = R.COMPANY1<EB.COM.MNEMONIC>

        FN.RE.CRF.MBGL = 'F':YMNEMONIC:'.RE.CRF.MBGL'
        SEL.CMD = ''; SEL.LIST = ''; NO.OF.REC = ''; SEL.ERR = ''
        SEL.CMD = "SELECT ":FN.RE.CRF.MBGL:" WITH DESC.1 LIKE 12... OR DESC.1 LIKE 2... OR DESC.1 LIKE 17..."
        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,SEL.ERR)
        SEL.FIN.LST<-1> = SEL.LIST
    REPEAT
    CALL BATCH.BUILD.LIST("",SEL.FIN.LST)
RETURN

INIT:
*****
    SEL.COMP = ''; SEL.CLIST = ''; REC.CLST = ''; SEL.LIST = ''
    SEL.ERR = ''; SEL.FIN.LST = ''; CMP.POSN = ''
RETURN
END
