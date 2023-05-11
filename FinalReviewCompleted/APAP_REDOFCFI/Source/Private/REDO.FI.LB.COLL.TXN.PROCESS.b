* @ValidationCode : MjotMTgyMjA0NjM2OTpVVEYtODoxNjgzNjE2MDk1NzM4OklUU1M6LTE6LTE6NjE4OjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 09 May 2023 12:38:15
* @ValidationInfo : Encoding          : UTF-8
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 618
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FI.LB.COLL.TXN.PROCESS(DATA.IN,PMT.REF.ID)
* ====================================================================================
*    - Gets the information related to the AA specified in input parameter
*
*    - Generates BULK OFS MESSAGES to apply payments to corresponding AA
*
* ====================================================================================
*
* Subroutine Type : Multithreaded ROUTINE - PROCESS
* Attached to     : REDO.FI.COLLECT service
* Attached as     : Service
* Primary Purpose : Apply PAYMENTS reported in APAP-Planillas
*
*
* Incoming:
* ---------
*
*        PMT.REF.ID  -  Contains ID of record detail to be processed
*
*
* Outgoing:
* ---------
*
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos ODR-2010-03-0025
* Development by  : Adriana Velasco - TAM Latin America
* Date            : Nov. 26, 2010
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*04-04-2023          jayasurya H                     MANUAL R22 CODE CONVERSION            call routine format modified
*=======================================================================

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_TSA.COMMON
*
    $INSERT I_REDO.FI.VAR.LOAN.BILL.COMMON
    $INSERT I_REDO.FI.LB.GENERATE.DATA.COMMON
    $INSERT I_F.REDO.FI.LB.BPROC.DET
    $INSERT I_F.REDO.FI.LB.BPROC
    $USING APAP.TAM
*
*
*************************************************************************
*

    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB CHECK.PRELIM.CONDITIONS
    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END

*
RETURN
*
* ======
PROCESS:
* ======
*

    WMONTO.DESCONTAR = R.REDO.FI.LB.BATCH.PROCESS.DET<REDO.FI.LB.BPROC.DET.MONTO.DESCONTAR>
*    WNUEVO.BALANCE  = R.REDO.FI.LB.BATCH.PROCESS.DET<REDO.FI.LB.BPROC.DET.NUEVO.BALANCE>
    WNUEVO.BALANCE   = ABS(R.REDO.FI.LB.BATCH.PROCESS.DET<REDO.FI.LB.BPROC.DET.BALANCE>)
    WMONTO.APLICAR   = R.REDO.FI.LB.BATCH.PROCESS.DET<REDO.FI.LB.BPROC.DET.MNT.APLICAR>
    WEXCESO          = 0
*

    IF WMONTO.APLICAR EQ 0 OR NOT(WMONTO.APLICAR) THEN
        WMONTO.APLICAR   = WNUEVO.BALANCE
    END
    GOSUB B140.APLICA.COBRO

RETURN
*
* ================
B140.APLICA.COBRO:
* ================
*

    COMM.DEBIT.AMOUNT = WMONTO.APLICAR
    DATA.IN<3> = AA.ARR.ID
    DATA.IN<5> = COMM.DEBIT.AMOUNT
*CALL APAP.TAM.REDO.FI.LB.APPLY.PYMT(DATA.IN,DATA.OUT)
    CALL APAP.TAM.redoFiLbApplyPymt(DATA.IN,DATA.OUT) ;*MANUAL R22 CODE CONVERSION

    IF DATA.OUT<1> EQ "SUCCESS" THEN

        GOSUB B150.UPDATE.BPROC.DET
        GOSUB B160.ACUMULA.SOBRANTE
    END ELSE
        GOSUB UPDATE.FAILURE.MSG
    END

*
RETURN
* ====================
UPDATE.FAILURE.MSG:
* ====================
*
    YRET = "R 01 60"
    CALL F.READU(FN.REDO.FI.LB.BATCH.PROCESS.DET, PMT.REF.ID, R.REDO.FI.LB.BATCH.PROCESS.DET, F.REDO.FI.LB.BATCH.PROCESS.DET, Y.ERR2,YRET)
    R.REDO.FI.LB.BATCH.PROCESS.DET<REDO.FI.LB.BPROC.ERROR.MSG> = DATA.OUT<2>
    CALL F.WRITE(FN.REDO.FI.LB.BATCH.PROCESS.DET,PMT.REF.ID,R.REDO.FI.LB.BATCH.PROCESS.DET)

RETURN

* ====================
B150.UPDATE.BPROC.DET:
* ====================
*
    YRET = "R 01 60"

    CALL F.READU(FN.REDO.FI.LB.BATCH.PROCESS.DET, PMT.REF.ID, R.REDO.FI.LB.BATCH.PROCESS.DET, F.REDO.FI.LB.BATCH.PROCESS.DET, Y.ERR2,YRET)
*
    R.REDO.FI.LB.BATCH.PROCESS.DET<REDO.FI.LB.BPROC.DET.MNT.APLICAR> = WMONTO.APLICAR
    R.REDO.FI.LB.BATCH.PROCESS.DET<REDO.FI.LB.BPROC.DET.ESTADO>      = "PAGO"
    R.REDO.FI.LB.BATCH.PROCESS.DET<REDO.FI.LB.BPROC.DET.PAYMENT.REF> = DATA.OUT<2>
    R.REDO.FI.LB.BATCH.PROCESS.DET<REDO.FI.LB.BPROC.DET.ERROR.MSG>   = ''
*
    CALL F.WRITE(FN.REDO.FI.LB.BATCH.PROCESS.DET,PMT.REF.ID,R.REDO.FI.LB.BATCH.PROCESS.DET)
*
RETURN
*
* ====================
B160.ACUMULA.SOBRANTE:
* ====================
*
    YRET = "R 01 20"
    PMT.SEQ.ID = FIELD(PMT.REF.ID,'.',1):'.': FIELD(PMT.REF.ID,'.',2)
    CALL F.READU(FN.REDO.FI.LB.BATCH.PROCESS,PMT.SEQ.ID,R.REDO.FI.LB.BATCH.PROCESS,F.REDO.FI.LB.BATCH.PROCESS,YER.BP,YRET)
    R.REDO.FI.LB.BATCH.PROCESS<REDO.FI.LB.BPROC.MONTO.TOTAL.PROC> + = WMONTO.APLICAR
    R.REDO.FI.LB.BATCH.PROCESS<REDO.FI.LB.BPROC.TOTAL.REG.PROC> + = 1
    CALL F.WRITE(FN.REDO.FI.LB.BATCH.PROCESS,PMT.SEQ.ID,R.REDO.FI.LB.BATCH.PROCESS)
*
RETURN
* =========
INITIALISE:
* =========
*
    LOOP.CNT                  = 1
    MAX.LOOPS                 = 1
*
RETURN
*
*
* =========
OPEN.FILES:
* =========
*

RETURN
*
* ======================
CHECK.PRELIM.CONDITIONS:
* ======================
*
    CALL F.READ(FN.REDO.FI.LB.BATCH.PROCESS.DET, PMT.REF.ID, R.REDO.FI.LB.BATCH.PROCESS.DET, F.REDO.FI.LB.BATCH.PROCESS.DET, Y.ERR2)
    AA.ARR.ID = R.REDO.FI.LB.BATCH.PROCESS.DET<REDO.FI.LB.BPROC.DET.ID.PRESTAMO>
RETURN
*

END
