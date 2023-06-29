import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspDPMM"
	title		: qsTr("Infinite mixture model")
	description	: qsTr("This module offers clustering analyses/density estimation and predictions on new data.")
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"

	GroupTitle
	{
		title:	qsTr("Dirichlet Process Mixture Model (DPMM)")
	}

	Analysis
	{
		title:	qsTr("Univariate Gaussian DPMM")
		func:	"univariateGaussianDPMM"
	}
}
