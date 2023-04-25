*========================================================================
*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.VERIFY.AZ.STATUS2
*========================================================================
* Technical report:
* =================
* Company Name   : APAP
* Program Name   : LAPAP.VERIFY.AZ.STATUS2
* Date           : 2018-05-21
* Item ID        : CN004475
*========================================================================
* Brief description :
* -------------------
* This program verify in 360 view whether exists or not a second status
* field filled in customer table, replacing in the ENQ first value.
*========================================================================
* Modification History :
* ======================
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2018-05-21     Richard HC                Initial Development
*========================================================================
* Content summary :
* =================
* Table name     : CUSTOMER
* Auto Increment : N/A
* Views/versions : 360 VIEW
* EB record      : LAPAP.VERIFY.AZ.STATUS2
* Routine        : LAPAP.VERIFY.AZ.STATUS2
*========================================================================

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_F.EB.LOOKUP
    $INSERT T24.BP I_ENQUIRY.COMMON

    ACC = O.DATA

    FN.EBL = "F.EB.LOOKUP"
    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    F.EBL = ""

    CALL OPF(FN.EBL,F.EBL)
    CALL OPF(FN.ACC,F.ACC)
    CALL F.READ(FN.ACC,ACC,R.ACC,F.ACC,E.ACC)

    CALL GET.LOC.REF("ACCOUNT","L.AC.STATUS1",POS1)
    CALL GET.LOC.REF("ACCOUNT","L.AC.STATUS2",POS2)
    FLD1 = R.ACC<AC.LOCAL.REF,POS1>
    FLD2 = R.ACC<AC.LOCAL.REF,POS2>

    CALL LAPAP.VERIFY.CATEGORY(ACC,CTG)
    IF CTG EQ 2 THEN

        IF FLD2 # "" THEN

            M = DCOUNT(FLD2,@SM)
            FOR A = 1 TO M STEP 1

                MM =  R.ACC<AC.LOCAL.REF,POS2,1>
                UU = R.ACC<AC.LOCAL.REF,POS2,2>
                PP = R.ACC<AC.LOCAL.REF,POS2,3>

                EBL = "L.AC.STATUS2*":MM
                OBL = "L.AC.STATUS2*":UU
                HC = "L.AC.STATUS2*":PP

                CALL F.READ(FN.EBL,EBL,R.EBL,F.EBL,E.EBL)
                EBLOOKUP = R.EBL<EB.LU.DESCRIPTION>
                CALL F.READ(FN.EBL,OBL,R.EBL,F.EBL,E.EBL)
                EBLOOKUP3 = R.EBL<EB.LU.DESCRIPTION>
                CALL F.READ(FN.EBL,HC,R.EBL,F.EBL,E.EBL)
                EBLOOKUP2 = R.EBL<EB.LU.DESCRIPTION>

            NEXT A

            IF M # 1 THEN
                O.DATA = EBLOOKUP<1,2>:@VM:EBLOOKUP3<1,2>:@VM:EBLOOKUP2<1,2>
            END ELSE
                O.DATA=EBLOOKUP<1,2>
            END
            RETURN

        END


        ELSE

            PP = DCOUNT(FLD1,@SM)
            FOR B = 1 TO PP STEP 1

                II = R.ACC<AC.LOCAL.REF,POS1,B>
                UBL="L.AC.STATUS1*":II
                CALL F.READ(FN.EBL,UBL,R.EBL,F.EBL,E.EBL)
                EBLOOK = R.EBL<EB.LU.DESCRIPTION>

            NEXT B
            O.DATA = EBLOOK<1,2>
            RETURN

        END

    END ELSE

        PP = DCOUNT(FLD1,@SM)
        FOR B = 1 TO PP STEP 1

            II = R.ACC<AC.LOCAL.REF,POS1,B>
            UBL="L.AC.STATUS1*":II
            CALL F.READ(FN.EBL,UBL,R.EBL,F.EBL,E.EBL)
            EBLOOK = R.EBL<EB.LU.DESCRIPTION>

        NEXT B
        O.DATA = EBLOOK<1,2>
        RETURN

    END


END
