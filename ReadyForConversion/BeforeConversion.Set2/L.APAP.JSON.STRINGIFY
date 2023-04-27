*-----------------------------------------------------------------------------
* <Rating>-1</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE  L.APAP.JSON.STRINGIFY(JSON.IN, JSON.OUT)
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE

*STRINGIFY subroutine.
*----------------------------------------------------------------------------------------------------------------------------------------------------
    JSON.OUT = JSON.IN
*TAB
    CHANGE CHAR(09) TO ''  IN JSON.OUT
*LINE FEED
    CHANGE CHAR(10) TO ''  IN JSON.OUT
*CARRIAGE RETURN
    CHANGE CHAR(13) TO ''  IN JSON.OUT
*REMOVE EXTRA SPACES
    JSON.OUT = TRIM(JSON.OUT, ' ','R')
*DELETE SPACES
    CHANGE '" : "' TO '":"'  IN JSON.OUT
    CHANGE '" , "' TO '","'  IN JSON.OUT

    CHANGE '": "' TO '":"'  IN JSON.OUT
    CHANGE '", "' TO '","'  IN JSON.OUT

    CHANGE '" :"' TO '":"'  IN JSON.OUT
    CHANGE '" ,"' TO '","'  IN JSON.OUT

    CHANGE '" : {' TO '":{'  IN JSON.OUT
    CHANGE '" : [' TO '":['  IN JSON.OUT

    CHANGE '": {' TO '":{'  IN JSON.OUT
    CHANGE '": [' TO '":['  IN JSON.OUT

    CHANGE '" :{' TO '":{'  IN JSON.OUT
    CHANGE '" :[' TO '":['  IN JSON.OUT

    CHANGE '} , "' TO '},"'  IN JSON.OUT
    CHANGE '] , "' TO '],"'  IN JSON.OUT

    CHANGE '}, "' TO '},"'  IN JSON.OUT
    CHANGE '], "' TO '],"'  IN JSON.OUT

    CHANGE '} ,"' TO '},"'  IN JSON.OUT
    CHANGE '] ,"' TO '],"'  IN JSON.OUT

    CHANGE '{ ""' TO '{""'   IN JSON.OUT
    CHANGE '" }' TO '"}""'   IN JSON.OUT

    CHANGE '[ ""' TO '[""'   IN JSON.OUT
    CHANGE '" "]' TO '"]""'   IN JSON.OUT
    RETURN
END
