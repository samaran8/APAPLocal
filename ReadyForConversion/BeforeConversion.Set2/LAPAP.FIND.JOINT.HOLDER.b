*-----------------------------------------------------------------------------
* <Rating>-8</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.FIND.JOINT.HOLDER

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.ACCOUNT

    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    CALL OPF(FN.ACC,F.ACC)

    FN.HIS = "F.ACCOUNT$HIS"
    F.HIS = ""
    CALL OPF(FN.HIS,F.HIS)

*-----------------------
* Verifying Account type
*-----------------------

    ACC = COMI
    CALL LAPAP.VERIFY.ACC(ACC,RES)
    Y.ACC.ID = RES

* DEBUG

*-------------------------
* If Account is a reinvest
*-------------------------


    IF ACC NE Y.ACC.ID THEN

        CALL F.READ(FN.ACC,Y.ACC.ID,R.ACC,F.ACC,ERR)
        JOINT.HOLDER = R.ACC<AC.JOINT.HOLDER,1>
        RELATION.CODE = R.ACC<AC.RELATION.CODE,1>

        IF RELATION.CODE EQ 500 OR RELATION.CODE EQ 501 THEN
            COMI = JOINT.HOLDER
        END ELSE
*           COMI = ""
            CALL LAPAP.CUSTOMER.NON.JOINT.HOLDER(Y.ACC.ID,RES)
            COMI = RES
        END

        RETURN


    END ELSE

*---------------------------
* If account is not reinvest
*---------------------------

        CALL F.READ(FN.ACC,ACC,R.ACC,F.ACC,ERR.noreinv)
        JOINT.HOLDER = R.ACC<AC.JOINT.HOLDER,1>
        RELATION.CODE = R.ACC<AC.RELATION.CODE,1>

        IF RELATION.CODE EQ 500 OR RELATION.CODE EQ 501 THEN
            COMI = JOINT.HOLDER
        END ELSE
*           COMI = ""
            CALL LAPAP.CUSTOMER.NON.JOINT.HOLDER(ACC,RES)
            COMI = RES
        END



        IF ERR.noreinv THEN

*---------------------------------------------
* If account not reinvest is closed via teller
*---------------------------------------------

            CALL F.READ.HISTORY(FN.HIS,ACC,R.HIS,F.HIS,ERRH)
            JOINT.HOLDER.HIS = R.HIS<AC.JOINT.HOLDER,1>
            RELATION.CODE.HIS = R.HIS<AC.RELATION.CODE,1>

            IF RELATION.CODE.HIS EQ 500 OR RELATION.CODE.HIS EQ 501 THEN
                COMI = JOINT.HOLDER.HIS
            END ELSE
*               COMI = ""
                ACC = ACC[1,10]
                CALL LAPAP.CUSTOMER.NON.JOINT.HOLDER(ACC,RES)
                COMI = RES
            END
        END

    END


*   if ACC is reinvest and category type 6601 != 6010-6020
    IF NOT(ERR) AND ERR.noreinv AND ERRH THEN
        Y.ACC = Y.ACC.ID[1,10]
        CALL LAPAP.CUSTOMER.NON.JOINT.HOLDER(Y.ACC.ID,RES)
    END



END
