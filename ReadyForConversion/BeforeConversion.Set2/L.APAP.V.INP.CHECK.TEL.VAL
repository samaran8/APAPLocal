*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* <Rating>-29</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.V.INP.CHECK.TEL.VAL
*-------------------------------------------------------------------------------------
* Technical report:
* -----------------
* Company Name   : APAP
* Program Name   : L.APAP.V.INP.CHECK.TEL.VAL
* Author         : RichardHC
* Item ID        : CN007728
*-------------------------------------------------------------------------------------
* Description :
* ------------
* This program allow verify the telephone number taking some condictions
*-------------------------------------------------------------------------------------
* Modification History :
* ----------------------
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2017/12/1     RichardHC         Initial development
*-------------------------------------------------------------------------------------
* Content summary :
* -----------------
* Table name     : NONE
* Auto Increment : NONE
* Views/versions : CUSTOMER,REDO.CLIENTE.PF/CUSTOMER,REDO.CLIENTE.PF.MOD
* EB record      : L.APAP.V.INP.CHECK.TEL.VAL
* Routines       : L.APAP.V.INP.CHECK.TEL.VAL
*-------------------------------------------------------------------------------------
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER

    FN.CUSTOMER = "F.CUSTOMER"
    F.CUSTOMER = ""

    CALL OPF(FN.CUSTOMER,F.ACCOUNT)
    CALL GET.LOC.REF("CUSTOMER","L.CU.TEL.AREA",CUS.POS)
    CUSAREA = R.NEW(EB.CUS.LOCAL.REF)<1,CUS.POS>

    CALL GET.LOC.REF("CUSTOMER","L.CU.TEL.NO",CUS.POS1)
    CUSTEL = R.NEW(EB.CUS.LOCAL.REF)<1,CUS.POS1>

    VoA = 0; VoU = 0
    M = DCOUNT(CUSTEL,@SM)
    FOR A = 1 TO M
        CUSTEL2 = R.NEW(EB.CUS.LOCAL.REF)<1,CUS.POS1,A>
        IF CUSTEL2 AND LEN(CUSTEL2) NE 7 THEN
            VoU = 1
        END ELSE
            VoU = 0
        END
    NEXT A

    L = DCOUNT(CUSAREA,@SM)
    FOR Aa = 1 TO L
        CUSAREA2 = R.NEW(EB.CUS.LOCAL.REF)<1,CUS.POS,Aa>
        IF CUSAREA2 EQ 809 OR CUSAREA2 EQ 829 OR CUSAREA2 EQ 849 THEN
            VoA = 1
        END ELSE
            VoA = 0
        END
    NEXT Aa

    IF VoU EQ 1 AND VoA EQ 1 THEN
        ETEXT = "SOLO 7 DIGITOS PARA CODIGOS DE AREAS DOMINICANOS EN NUMEROS DE TELEFONOS"
        VoU = 0; VoA = 0
        CALL STORE.END.ERROR
        RETURN
    END

   CALL REBUILD.SCREEN
END

 *---------------------------------------------------------------------------------
 * This fragment of code is used for validate the duplicate number in a local field
 * enable it only if you are sure to do it, because the same can be affect services
 * and interfaces such as vision plus that don't implement this kind for validation
 *---------------------------------------------------------------------------------
 *   POO = ''
 *   CALL L.APAP.V.DUP.CHECK.TEL.VAL (POO)
 *   IF POO EQ "YES" THEN
 *       ETEXT = "POR FAVOR VERIFIQUE, EXISTEN NUMEROS DE TELEFONOS DUPLICADOS"
 *       CALL STORE.END.ERROR
 *       RETURN
 *   END
 *--------------------------------------------------------------------------------
 * For unable it copy and paste the below code putting the same name:
 *--------------------------------------------------------------------------------
 *   SUBROUTINE L.APAP.V.DUP.CHECK.TEL.VAL (POO)
 *
 *   $INSERT T24.BP I_COMMON
 *   $INSERT T24.BP I_EQUATE
 *   $INSERT T24.BP I_F.CUSTOMER
 *
 *   FN.CUSTOMER = "F.CUSTOMER"
 *   F.CUSTOMER = ""
 *
 *
 *   CALL OPF(FN.CUSTOMER,F.ACCOUNT)
 *
 *   CALL GET.LOC.REF("CUSTOMER","L.CU.TEL.NO",CUS.POS)
 *   CUSTEL = R.NEW(EB.CUS.LOCAL.REF)<1,CUS.POS>
 *
 *   M = DCOUNT(CUSTEL,@SM)
 *
 *   FOR A = 1 TO M STEP 1
 *       FOR Ax = 2 TO M STEP 1
 *           IF Ax NE A THEN
 *               CUSTEL2 = R.NEW(EB.CUS.LOCAL.REF)<1,CUS.POS,A>
 *               CUSTEL3 = R.NEW(EB.CUS.LOCAL.REF)<1,CUS.POS,Ax>
 *               IF (CUSTEL2 NE '' AND CUSTEL3 NE '' AND CUSTEL3 EQ CUSTEL2) THEN
 *                   POO = "YES"
 *               END
 *
 *           END
 *       NEXT Ax
 *   NEXT A
 *   CALL REBUILD.SCREEN
 * END
*------------------------------------------------------------------------------------
