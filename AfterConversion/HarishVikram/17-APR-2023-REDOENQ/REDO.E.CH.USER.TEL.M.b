* @ValidationCode : MjotMTY3OTUyMzg4MDpDcDEyNTI6MTY4MTcyMDIyMjAyODpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 17 Apr 2023 14:00:22
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
$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.CH.USER.TEL.M(R.DATA)
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
* This is an Nofile routine to get some data for enquiry REDO.E.CH.USER.INT.M
* related to personal channel users
*
* Input/Output:
*--------------
* IN : USER.ID / CUSTOMER.ID
* OUT : R.DATA (ALL DATA)
*---------------
*-----------------------------------------------------------------------------
* Modification History :
*   Date            Who                   Reference               Description
* 14-AUG-2012    RMONDRAGON              ODR-2011-06-0243        FIRST VERSION
* 09-APR-2013    RMONDRAGON              ODR-2011-06-0243        SECOND VERSION
* 22-MAY-2015    Ashokkumar              PACS00459377            Select changed for Performance improve.
* 17-APR-2023     Conversion tool   R22 Auto conversion   	 VM to @VM, F.READ to CACHE.READ
* 17-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
* <region name= Inserts>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.EB.EXTERNAL.USER
    $INSERT I_F.CUST.DOCUMENT
    $INSERT I_F.DOCUMENT.STATUS

    $INSERT I_F.REDO.CH.PROFILE

* </region>
*-----------------------------------------------------------------------------

    GOSUB OPENFILES
    GOSUB PROCESS

RETURN

*********
OPENFILES:
*********

    FN.EB.EXTERNAL.USER = 'F.EB.EXTERNAL.USER'
    F.EB.EXTERNAL.USER = ''
    CALL OPF(FN.EB.EXTERNAL.USER,F.EB.EXTERNAL.USER)

    FN.REDO.CH.PROFILE = 'F.REDO.CH.PROFILE'
    F.REDO.CH.PROFILE = ''
    CALL OPF(FN.REDO.CH.PROFILE,F.REDO.CH.PROFILE)

    FN.CUST.DOCUMENT = 'F.CUST.DOCUMENT'
    F.CUST.DOCUMENT = ''
    CALL OPF(FN.CUST.DOCUMENT,F.CUST.DOCUMENT)

    FN.DOCUMENT.STATUS = 'F.DOCUMENT.STATUS'
    F.DOCUMENT.STATUS  = ''
    CALL OPF(FN.DOCUMENT.STATUS,F.DOCUMENT.STATUS)

RETURN

********
PROCESS:
********

    LOCATE 'USER.ID' IN D.FIELDS<1> SETTING USER.ID.POS THEN
        Y.USER.ID = D.RANGE.AND.VALUE<USER.ID.POS>
    END

    LOCATE 'CUSTOMER.ID' IN D.FIELDS<1> SETTING CUSTOMER.ID.POS THEN
        Y.CUSTOMER.ID = D.RANGE.AND.VALUE<CUSTOMER.ID.POS>
    END

    IF Y.USER.ID NE '' AND Y.CUSTOMER.ID EQ '' THEN
*        Y.SEL.CMD = 'SSELECT ':FN.EB.EXTERNAL.USER:' WITH @ID LIKE ':Y.USER.ID:' AND CHANNEL EQ TELEFONO AND PROD.USED EQ PERSONAL.TEL'
        Y.SEL.CMD = 'SSELECT ':FN.EB.EXTERNAL.USER:' WITH @ID EQ ':Y.USER.ID:' AND PROD.USED EQ PERSONAL.TEL'
    END

    IF Y.CUSTOMER.ID NE '' AND Y.USER.ID EQ '' THEN
*        Y.SEL.CMD = 'SSELECT ':FN.EB.EXTERNAL.USER:' WITH CUSTOMER LIKE ':Y.CUSTOMER.ID:' AND CHANNEL EQ TELEFONO AND PROD.USED EQ PERSONAL.TEL'
        Y.SEL.CMD = 'SSELECT ':FN.EB.EXTERNAL.USER:' WITH CUSTOMER EQ ':Y.CUSTOMER.ID:' AND PROD.USED EQ PERSONAL.TEL'
    END

    IF Y.USER.ID EQ '' AND Y.CUSTOMER.ID EQ '' THEN
*        Y.SEL.CMD = 'SSELECT ':FN.EB.EXTERNAL.USER:' WITH CHANNEL EQ TELEFONO AND PROD.USED EQ PERSONAL.TEL'
    END

    SEL.CMD.ERR = ''
    CALL EB.READLIST(Y.SEL.CMD,Y.SEL.CMD.LIST,'',Y.SEL.CMD.LIST.NO,SEL.CMD.ERR)

    CNT.REC = 1
    LOOP
    WHILE CNT.REC LE Y.SEL.CMD.LIST.NO DO
        Y.REC.ID = Y.SEL.CMD.LIST<CNT.REC>
        R.EB.EXTERNAL.USER = '' ; EEU.ERR = ''; Y.CHANNEL = ''
        CALL CACHE.READ(FN.EB.EXTERNAL.USER, Y.REC.ID, R.EB.EXTERNAL.USER, EEU.ERR) ;*R22 Auto conversion
        IF NOT(R.EB.EXTERNAL.USER) THEN
            CONTINUE
        END
        GOSUB GET.PROFILE
        IF Y.PROFILE NE '' THEN
            Y.NAME = R.EB.EXTERNAL.USER<EB.XU.NAME>
            Y.CHANNEL = R.EB.EXTERNAL.USER<EB.XU.CHANNEL>
            IF Y.CHANNEL NE 'TELEFONO' THEN
                CONTINUE
            END
            Y.CUST = R.EB.EXTERNAL.USER<EB.XU.CUSTOMER>
            Y.STATUS = R.EB.EXTERNAL.USER<EB.XU.STATUS>
            Y.ARR = R.EB.EXTERNAL.USER<EB.XU.ARRANGEMENT>
            GOSUB GET.CHCTTO
            R.DATA<-1> := Y.REC.ID:'*':Y.NAME:'*':Y.CHANNEL:'*':Y.CUST:'*':Y.STATUS:'*':Y.ARR:'*':Y.PROFILE:'*':Y.CHCTTO.STATUS
        END
        CNT.REC += 1
    REPEAT

RETURN

************
GET.PROFILE:
************

    Y.PROFILE = '' ; R.REDO.CH.PROFILE = '' ; PROF.ERR = ''
    CALL F.READ(FN.REDO.CH.PROFILE,Y.REC.ID,R.REDO.CH.PROFILE,F.REDO.CH.PROFILE,PROF.ERR)
    IF R.REDO.CH.PROFILE THEN
        Y.PROFILE = R.REDO.CH.PROFILE<REDO.CH.PROF.PROFILE>
    END

    IF Y.PROFILE EQ 'Teleapap.Consultas' THEN
        IF LNGG EQ 1 THEN
            Y.PROFILE = 'CONSULTS'
        END ELSE
            Y.PROFILE = 'CONSULTAS'
        END
    END

    IF Y.PROFILE EQ 'Teleapap.Txns' THEN
        IF LNGG EQ 1 THEN
            Y.PROFILE = 'TRANSACTIONS'
        END ELSE
            Y.PROFILE = 'TRANSACCIONES'
        END
    END

RETURN

***********
GET.CHCTTO:
***********

    Y.CHCTTO.STATUS.ID = ''
    Y.CHCTTO.STATUS = ''

    Y.CHCTTO.ID = Y.CUST:'*CANIVR'
    R.CUST.DOCUMENT = '' ; CD.ERR = ''
    CALL F.READ(FN.CUST.DOCUMENT,Y.CHCTTO.ID,R.CUST.DOCUMENT,F.CUST.DOCUMENT,CD.ERR)
    IF R.CUST.DOCUMENT THEN
        Y.CHCTTO.STATUS.ID = R.CUST.DOCUMENT<CUS.DOC.STATUS>
    END

    R.DOCUMENT.STATUS = ''; DS.ERR = ''
    CALL F.READ(FN.DOCUMENT.STATUS,Y.CHCTTO.STATUS.ID,R.DOCUMENT.STATUS,F.DOCUMENT.STATUS,DS.ERR)
    IF R.DOCUMENT.STATUS THEN
        Y.CHCTTO.STATUS = R.DOCUMENT.STATUS<DOC.STAT.DESCRIPTION>
        IF LNGG EQ 1 THEN
            Y.CHCTTO.STATUS = FIELD(Y.CHCTTO.STATUS,@VM,1)
        END ELSE
            Y.CHCTTO.STATUS = FIELD(Y.CHCTTO.STATUS,@VM,2)
        END
    END

RETURN

END
