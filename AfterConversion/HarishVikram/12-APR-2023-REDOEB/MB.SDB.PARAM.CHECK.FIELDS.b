* @ValidationCode : MjoxOTU4MDIxMzc0OkNwMTI1MjoxNjgxMjkxMzQ1NTQ0OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOnRydWU6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 12 Apr 2023 14:52:25
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : true
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOEB
SUBROUTINE MB.SDB.PARAM.CHECK.FIELDS

*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 12-APR-2023     Conversion tool   R22 Auto conversion  	 VM to @VM, = to EQ
* 12-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CATEGORY
    $INSERT I_F.TRANSACTION
    $INSERT I_F.APPL.GEN.CONDITION
    $INSERT I_F.MB.SDB.PARAM

    FN.ACCOUNT = "F.ACCOUNT"
    F.ACCOUNT = ""
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

    FN.TRANSACTION = "F.TRANSACTION"
    F.TRANSACTION = ""
    CALL OPF(FN.TRANSACTION,F.TRANSACTION)

    FN.APPL.GEN.COND = 'F.APPL.GEN.CONDITION'; F.APPL.GEN.COND = ''
    CALL OPF(FN.APPL.GEN.COND, F.APPL.GEN.COND)

    APPL.ID = 'MB.SDB.STATUS'; APPL.REC = ''; APPL.ERR = ''
    CALL CACHE.READ(FN.APPL.GEN.COND, APPL.ID, APPL.REC, APPL.ERR) ;*R22 Auto conversion

    BEGIN CASE
        CASE AF EQ SDB.PAR.VAT.PERCENT
            GOSUB VAT.PERCENT.CHECK

        CASE AF EQ SDB.PAR.STAFF.GROUP
            GOSUB STAFF.GROUP.CHECK

        CASE AF EQ SDB.PAR.DISC.GROUP
            GOSUB DISC.GROUP.CHECK

        CASE AF EQ SDB.PAR.DISC.FLAT.AMT
            GOSUB DISC.FLAT.AMT.CHECK

        CASE AF EQ SDB.PAR.DISC.PERCENT
            GOSUB DISC.PERCENT.CHECK

        CASE AF EQ SDB.PAR.RENEW.FREQUENCY
            GOSUB RENEW.FREQUENCY.CHECK

        CASE AF EQ SDB.PAR.RENEW.NOTICE.FREQ OR AF EQ SDB.PAR.2ND.REMINDER.FREQ
            GOSUB CHECK.FREQ

    END CASE

RETURN

VAT.PERCENT.CHECK:

    VAT.PERC = COMI

    IF VAT.PERC GE 100 THEN
        E = "Value Should be LT 100"
    END

RETURN

DISC.GROUP.CHECK:

    IF COMI THEN
        LOCATE COMI IN APPL.REC<AGC.CONTRACT.GRP, 1> SETTING POS ELSE
            E = "Invalid Customer Group For Safety Deposit Box"
        END
    END

RETURN

STAFF.GROUP.CHECK:

    IF COMI THEN
        LOCATE COMI IN APPL.REC<AGC.CONTRACT.GRP, 1> SETTING POS ELSE
            E = "Invalid Staff Group For Safety Deposit Box"
        END
    END

RETURN

DISC.FLAT.AMT.CHECK:

    VAT.PERC = COMI

    IF COMI AND R.NEW(SDB.PAR.DISC.PERCENT)<1,AV> GT 0 THEN
        E = 'Either Flat amount or percentage should be defined'
    END

RETURN

DISC.PERCENT.CHECK:

    VAT.PERC = COMI

    IF VAT.PERC GE 100 THEN
        E = "Value Should be LT 100"
    END

    IF COMI AND R.NEW(SDB.PAR.DISC.FLAT.AMT)<1,AV> GT 0 THEN
        E = 'Either Flat amount or percentage should be defined'
    END

RETURN

RENEW.FREQUENCY.CHECK:

    IF NOT(COMI) THEN
        RETURN
    END

    IF NOT(R.NEW(SDB.PAR.FREQ.TYPE)) THEN
        E = 'First choose the frequency type, Yearly or Monthly?'
    END

    IF R.NEW(SDB.PAR.FREQ.TYPE) EQ 'YEARLY' AND COMI AND COMI[9,3] NE 'M12' THEN
        E = 'Frequency Type and Renewal frequency do not match'
    END

    IF R.NEW(SDB.PAR.FREQ.TYPE) EQ 'MONTHLY' AND COMI AND COMI[9,3] NE 'M01' THEN
        E = 'Frequency Type and Renewal frequency do not match'
    END

RETURN

CHECK.FREQ:

    IF COMI THEN
        INPUT.FREQ = COMI
        SET.ERR.FLG = 0
        REST.OF.CHAR = 0

        LAST.CHAR = INPUT.FREQ[LEN(INPUT.FREQ),1]
        IF LAST.CHAR MATCHES 'W':@VM:'C' THEN
            REST.OF.CHAR = INPUT.FREQ[1,LEN(INPUT.FREQ)-1]
            IF NUM(REST.OF.CHAR) THEN
                IF REST.OF.CHAR GT 99 THEN
                    SET.ERR.FLG = 1
                END
            END ELSE
                SET.ERR.FLG = 1
            END
        END ELSE
            SET.ERR.FLG = 1
        END

        IF SET.ERR.FLG EQ 1 THEN
            E = 'Invalid Frequency. E.g. 15C, 10W, etc'
        END
    END

RETURN

END
