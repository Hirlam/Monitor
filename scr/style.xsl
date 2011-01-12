<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:template match="/">
	<html>
	<body>
		<h1> Station statistics for selection: <xsl:value-of select="STAT/TAG"/></h1>
		<h1> Period <xsl:value-of select="STAT/PERIOD"/>  </h1>
		<h1> Parameter: 
		     <xsl:value-of select="STAT/VAR"/> 
		     (<xsl:value-of select="STAT/UNIT"/>)
		     sorted by RMSE </h1>
	<table border="1">
        <tr>
        <th>STATION</th>
        <th>EXP</th>
        <th>NUM</th>
        <th>BIAS</th>
        <th>RMSE</th>
        <th>STDV</th>
        <th>CORR</th>
        </tr>
	<xsl:for-each select="STAT/STATION/EXP">
	<xsl:sort select="RMSE" data-type="number" order="descending"/>
	<tr>
	<td><xsl:value-of select="ID"/></td>
	<td><xsl:value-of select="NAME"/></td>
	<td><xsl:value-of select="NUM"/></td>
	<td><xsl:value-of select="BIAS"/></td>
	<td><xsl:value-of select="RMSE"/></td>
	<td><xsl:value-of select="STDV"/></td>
	<td><xsl:value-of select="CORR"/></td>
	</tr>
	</xsl:for-each>
	</table>
	</body>
	</html>
        </xsl:template>
</xsl:stylesheet>
