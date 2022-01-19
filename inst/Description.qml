import QtQuick		2.12
import JASP.Module	1.0

Description
{
    name		: "jaspGLM"
    title		: qsTr("GLM")
    description	: qsTr("This module offers analyses based on generalized linear models.")
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"

    GroupTitle
    {
        title: qsTr("Classical")
    }

    Analysis
    {
        title: qsTr("Generalized Linear Models")
        func: "glmClassical"
    }
}
