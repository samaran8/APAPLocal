*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.TT.DEAL.RATE.FMT(YFIELD.NME)

***********************************************************************************
* Description: This deal slip routine will validate the deal rate in the TELLER.
*
* Dev by : V.P.Ashokkumar
*
************************************************************************************
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE T24.BP I_F.TELLER

    IF YFIELD.NME NE 'Y.DEAL.RATE' THEN
        RETURN
    END

    Y.DEAL.RATE = R.NEW(TT.TE.DEAL.RATE)
    IF INDEX(Y.DEAL.RATE,".",1) THEN
        YFIELD.NME = FMT(Y.DEAL.RATE,'R2#7')
    END ELSE
        YFIELD.NME = FMT(Y.DEAL.RATE,'R2#7')
    END
    RETURN
END
