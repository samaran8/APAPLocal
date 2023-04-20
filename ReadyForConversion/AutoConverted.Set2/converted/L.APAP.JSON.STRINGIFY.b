SUBROUTINE  L.APAP.JSON.STRINGIFY(JSON.IN, JSON.OUT)
    $INSERT I_COMMON
    $INSERT I_EQUATE

*STRINGIFY subroutine.
*----------------------------------------------------------------------------------------------------------------------------------------------------
    JSON.OUT = JSON.IN
*TAB
    CHANGE CHARX(09) TO ''  IN JSON.OUT
*LINE FEED
    CHANGE CHARX(10) TO ''  IN JSON.OUT
*CARRIAGE RETURN
    CHANGE CHARX(13) TO ''  IN JSON.OUT
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
