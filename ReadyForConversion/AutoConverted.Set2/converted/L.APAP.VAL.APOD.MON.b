* Rutina de validacion para verificar el campo JOINT.HOLDER, solo envie el valor
*  YES a cuando la relacion de apoderado sea relacion  530 APODERADO 1  y  531 APODERADO 2

SUBROUTINE L.APAP.VAL.APOD.MON
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.RELATION

    Y.ACC.ID = COMI

    GOSUB OPEN.TABLE
    GOSUB READ.TABLE.ACCOUNT
RETURN
OPEN.TABLE:
    FN.ACC = "F.ACCOUNT"
    FV.ACC = ""
    CALL OPF(FN.ACC,FV.ACC)

    FN.ACCHIS = "F.ACCOUNT$HIS"
    FV.ACCHIS = ""
    CALL OPF(FN.ACC,FV.ACC)
RETURN
READ.TABLE.ACCOUNT:
    CALL F.READ(FN.ACC,Y.ACC.ID,R.ACC,FV.ACC,ACC.ERROR)
    Y.CANT.RELACIONES = R.ACC<AC.JOINT.HOLDER>
    Y.RELATION.CODE = R.ACC<AC.RELATION.CODE>
    Y.RELATION.CODE = CHANGE (Y.RELATION.CODE,@SM,@FM)
    Y.RELATION.CODE = CHANGE (Y.RELATION.CODE,@VM,@FM)
    Y.AC.CURR.NO =  R.ACC<AC.CURR.NO>
    COMI = 'NO'
    IF Y.AC.CURR.NO EQ  1  THEN
        IF  Y.CANT.RELACIONES NE '' THEN
            CNT = DCOUNT(Y.RELATION.CODE,@FM)
            FOR A = 1 TO CNT
                IF Y.RELATION.CODE<A> EQ '530' OR Y.RELATION.CODE<A> EQ '531' THEN
                    COMI = 'YES'
                    RETURN
                END
            NEXT A
        END
    END

    IF Y.AC.CURR.NO GT 1 THEN
        GOSUB READ.HISTORICO
    END

RETURN

READ.HISTORICO:
    Y.CURR.ANTERIOR = Y.AC.CURR.NO - 1
    Y.ACC.IDHIS = Y.ACC.ID:";":Y.CURR.ANTERIOR
    CALL F.READ(FN.ACCHIS,Y.ACC.IDHIS,R.ACCHIS,FV.ACCHIS,ACC.ERROR)
    Y.CANT.RELACIONESHIS = R.ACCHIS<AC.JOINT.HOLDER>
    Y.CANT.RELACIONES = CHANGE(Y.CANT.RELACIONES,@VM,@FM)
    Y.CANT.RELACIONES = CHANGE(Y.CANT.RELACIONES,@SM,@FM)
    Y.CANT.RELACIONESHIS = CHANGE(Y.CANT.RELACIONESHIS,@VM,@FM)
    Y.RELATION.CODE.HIS = R.ACCHIS<AC.RELATION.CODE>
    Y.RELATION.CODE.HIS = CHANGE (Y.RELATION.CODE.HIS,@SM,@FM)
    Y.RELATION.CODE.HIS = CHANGE (Y.RELATION.CODE.HIS,@VM,@FM)
    Y.CNT = DCOUNT(Y.CANT.RELACIONES,@FM)

    FOR I.VAR = 1 TO Y.CNT
        Y.CODIGO.CLIENTE = Y.CANT.RELACIONES<I.VAR>
        Y.CODIGO.RELACION = Y.RELATION.CODE.HIS<I.VAR>

        IF Y.RELATION.CODE<I.VAR> NE '530' AND Y.RELATION.CODE<I.VAR> NE '531' THEN
            CONTINUE
        END

        IF Y.RELATION.CODE<I.VAR> EQ '530' OR Y.RELATION.CODE<I.VAR> EQ '531' THEN

            LOCATE Y.CODIGO.CLIENTE IN Y.CANT.RELACIONESHIS<1> SETTING Y.RELACION.POS THEN

                IF Y.RELATION.CODE<I.VAR> MATCHES Y.RELATION.CODE.HIS<Y.RELACION.POS> THEN
                    COMI = 'NO'
                END ELSE
                    COMI = 'YES'
                    RETURN
                END
            END ELSE
                COMI = 'YES'
                RETURN
            END
        END


    NEXT I.VAR

RETURN

END
