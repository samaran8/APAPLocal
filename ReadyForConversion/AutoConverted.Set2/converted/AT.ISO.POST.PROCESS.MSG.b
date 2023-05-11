SUBROUTINE AT.ISO.POST.PROCESS.MSG(ISO.MESSAGE)
*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AT.ISO.COMMON
    $INSERT I_F.ATM.PARAMETER
    $INSERT I_F.INTRF.MAPPING

    IF ISO.MESSAGE EQ 'SECURITY VIOLATION' OR ISO.MESSAGE EQ 'SECURITY VIOLATION DURING SIGN ON PROCESS' OR ISO.MESSAGE EQ 'VIOLACICN DE SEGURIDAD' OR AT$AT.RES.MAP.ID EQ '' OR AT$AT.RES.MAP.ID EQ '0'  THEN
        CALL LOAD.COMPANY('DO0010001')
        CALL ATM.ISO.SYS.ERR.RTN(ISO.MESSAGE)
        OFS.RESPONSE=ISO.MESSAGE
    END   ELSE

        CALL ATM.ISO.ERR.CODE.RTN(ISO.MESSAGE)
    END
    CALL RAD.LOG.MSG("ATM","DEBUG",ISO.MESSAGE)
    GOSUB INITIALISE
    CALL RAD.LOG.MSG("ATM","DEBUG",ISO.MESSAGE)
    GOSUB READ.MAPPING
    GOSUB FORM.RESPONSE
    ISO.MESSAGE = FMT(LEN(RESPONSE.MSG),'R%4'):RESPONSE.MSG
    CALL RAD.LOG.MSG("ATM","INFO",ISO.MESSAGE)

RETURN  ;* Main

*--------------------------------------------------------------------------------------------------------------------------*

INITIALISE:

    FN.INTRF.MAPPING = 'F.INTRF.MAPPING'
    F.INTRF.MAPPING  = ''
    CALL OPF(FN.INTRF.MAPPING,F.INTRF.MAPPING)
*added anitha
    FN.ATM.PARAMETER='F.ATM.PARAMETER'
* F.ATM.PARAMETER=''
* CALL OPF(FN.ATM.PARAMETER,F.ATM.PARAMETER)

    CALL CACHE.READ(FN.ATM.PARAMETER,'SYSTEM',R.ATM.PARAMETER,E.ATM.PARAMETER)
*END ANITHA
    OFS.RESPONSE = ISO.MESSAGE  ;*Incoming
    FLDS.PRESENT ='' ;FLD.ADDED =''

RETURN  ;*From initialise
*--------------------------------------------------------------------------------------------------------------------------*
READ.MAPPING:
*----------*

    RES.MAP.ID = AT$AT.RES.MAP.ID
    CALL RAD.LOG.MSG("ATM","DEBUG","Response mapping to use ":RES.MAP.ID)
    CALL CACHE.READ(FN.INTRF.MAPPING,RES.MAP.ID,R.MAPPING,ER.MAPPING)
    IF ER.MAPPING THEN


        RES.MAP.ID=AT$INCOMING.ISO.REQ(1)
        CALL CACHE.READ(FN.INTRF.MAPPING,RES.MAP.ID,R.MAPPING,ER.MAPPING)

    END ELSE
        CALL RAD.LOG.MSG("ATM","INFO","Missing Response mapping":RES.MAP.ID)

    END


RETURN  ;*From READ.MAPPING
*--------------------------------------------------------------------------------------------------------------------------*
FORM.RESPONSE:
*------------*

    RESPONSE.MSG = '' ; LOG.MSG =''
    CHANGE '"' TO '' IN ISO.MESSAGE
    LOG.MSG = "Response ISO Fields "
    CHANGE '"' TO  '' IN ISO.MESSAGE
    Y.CNT.MAP.RESP=   DCOUNT(R.MAPPING<INTRF.MAP.INTRF.FLD.NAME>,@VM)
    FOR II=0 TO Y.CNT.MAP.RESP

        BEGIN CASE
            CASE R.MAPPING<INTRF.MAP.FIELD.SOURCE,II> EQ 'CON'      ;* Constants
                FLD.VALUE =R.MAPPING<INTRF.MAP.GLO.CONSTANT,II>
                IF FLD.VALUE NE '' THEN
                    GOSUB ADD.FLD.VALUE.TO.RESP
                END

            CASE R.MAPPING<INTRF.MAP.FIELD.SOURCE,II> EQ 'ENQ.VAL' OR R.MAPPING<INTRF.MAP.FIELD.SOURCE,II> EQ 'EXT'   ;* OFS o/p

                FLD.NAME = R.MAPPING<INTRF.MAP.GLO.FLD.NAME,II>
                NO.OF.FIELDS = DCOUNT(OFS.RESPONSE,',')
                FOR JJ = 1 TO NO.OF.FIELDS
                    IF INDEX(FIELD(OFS.RESPONSE,',',JJ),FLD.NAME,1) THEN
                        FLD.VALUE=FIELD(FIELD(OFS.RESPONSE,',',JJ),'=',2)
                        IF FLD.VALUE NE '' THEN
                            GOSUB ADD.FLD.VALUE.TO.RESP
                        END
                    END
                NEXT JJ


            CASE R.MAPPING<INTRF.MAP.FIELD.SOURCE,II> EQ 'INT'      ;* Echo from original ISO request

                FLD.VALUE = AT$INCOMING.ISO.REQ(R.MAPPING<INTRF.MAP.INTRF.FLD.PS,II>)
                IF FLD.VALUE NE  ''  THEN
                    GOSUB ADD.FLD.VALUE.TO.RESP
                END
            CASE R.MAPPING<INTRF.MAP.FIELD.SOURCE,II> EQ 'RTN'
                FLD.RTN = R.MAPPING<INTRF.MAP.FIELD.SRC.VALUE,II>
                GLO.FLD.NAME = R.MAPPING<INTRF.MAP.GLO.FLD.NAME,II>
                FLD.MSG.POSN = R.MAPPING<INTRF.MAP.INTRF.FLD.PS,II,1>
                COMP.FLAG= '' ; CALL CHECK.ROUTINE.EXIST(FLD.RTN,COMP.FLAG,RET.INFO)
                GLO.FLD.NAME =FIELD(GLO.FLD.NAME,':',1)
                CALL RAD.ANALYSE.OFS.MSG(ISO.MESSAGE,GLO.FLD.NAME,FLD.VALUE)
                IF COMP.FLAG EQ '1' THEN
                    CALL @FLD.RTN(FLD.VALUE,RET.VALUE)
                    CALL RAD.LOG.MSG("ATM","DEBUG","Calling ":FLD.RTN :" ,":FLD.VALUE:" ,":RET.VALUE)
                END
                IF RET.VALUE NE '' THEN
                    FLD.VALUE = RET.VALUE
                    GOSUB ADD.FLD.VALUE.TO.RESP
                END

        END CASE

        IF FLD.ADDED THEN
            FLDS.PRESENT<1,-1> = R.MAPPING<INTRF.MAP.INTRF.FLD.PS,II>
        END
        FLD.VALUE ='' ; FLD.ADDED =''




    NEXT II

    CALL RAD.LOG.MSG("ATM","DEBUG",LOG.MSG)
    GOSUB FORM.BIT.MAP
    CALL RAD.LOG.MSG("ATM","INFO",LOG.MSG)

RETURN  ;*From formResponse
*--------------------------------------------------------------------------------------------------------------------------*
ADD.FLD.VALUE.TO.RESP:
*--------------------*
    IF INDEX(AT$ISO.MESSAGE.SKELETON(R.MAPPING<INTRF.MAP.INTRF.FLD.PS,II>),'LLL',1) THEN
        RESPONSE.MSG<R.MAPPING<INTRF.MAP.INTRF.FLD.PS,II>> = FMT(LEN(FLD.VALUE),'R%3'):FLD.VALUE
        IF RESPONSE.MSG<62> EQ '' AND AT$INCOMING.ISO.REQ(3)[1,2] EQ '35' THEN
            RESPONSE.MSG<62>='000'
        END
    END ELSE

        IF INDEX(AT$ISO.MESSAGE.SKELETON(R.MAPPING<INTRF.MAP.INTRF.FLD.PS,II>),'LL',1) THEN
            RESPONSE.MSG<R.MAPPING<INTRF.MAP.INTRF.FLD.PS,II>> = FMT(LEN(FLD.VALUE),'R%2'):FLD.VALUE
        END ELSE
            FLD.VALUE = TRIM(FLD.VALUE,'"','A')
            RESPONSE.MSG<R.MAPPING<INTRF.MAP.INTRF.FLD.PS,II>> = FLD.VALUE
        END
    END
    FLD.ADDED = 1
    LOG.MSG := "(":R.MAPPING<INTRF.MAP.INTRF.FLD.PS,II>:")(":FLD.VALUE:")"
RETURN  ;*From ADD.FLD.VALUE.TO.RESP
*--------------------------------------------------------------------------------------------------------------------------*

FORM.BIT.MAP:
*-----------*

    BIN.BIT.MAP ='' ; NO.OF.FLDS.PRESENT = DCOUNT(FLDS.PRESENT,@VM)

    IF FLDS.PRESENT<1,NO.OF.FLDS.PRESENT> GT 64 THEN
        TOT.FIELDS = 128          ;* With secondary bitmap
    END ELSE
        TOT.FIELDS = 64
    END

    FOR CNT= 1 TO TOT.FIELDS
        IF TOT.FIELDS EQ 64 AND CNT EQ 1 THEN
            BIN.BIT.MAP = 0
        END ELSE
            IF RESPONSE.MSG<CNT> NE '' THEN
                BIN.BIT.MAP :=1
            END ELSE
                BIN.BIT.MAP:= 0
            END
        END
    NEXT CNT


    HEX.BIT.MAP =''
    FOR II = 1 TO 128
        HEX.BIT.MAP:= OCONV(BIN.BIT.MAP[II,4],"MCBX")[2,1]
        II += 3
    NEXT II
    RESPONSE.MSG<1> := HEX.BIT.MAP
    CHANGE @FM TO '' IN RESPONSE.MSG

RETURN  ;*from FORM.BIT.MAP
*--------------------------------------------------------------------------------------------------------------------------*
