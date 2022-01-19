//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick			2.8
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

// All Analysis forms must be built with the From QML item
Form {
    id: form

    VariablesForm
    {
        preferredHeight: 350

        AvailableVariablesList
        {
            name:				"allVariablesList"
        }

        AssignedVariablesList
        {
            name:				"dependent"
            title:				qsTr("Dependent variable")
            allowedColumns:		["scale","ordinal", "nominal", "nominalText"]
            singleVariable:		true
        }

        AssignedVariablesList
        {
            visible:			family.currentText == "Binomial (aggregated)"
            onVisibleChanged:	if (!visible && count > 0) itemDoubleClicked(0);
            name:				"dependentAggregation"
            title:				qsTr("Number of trials")
            singleVariable:		true
            allowedColumns:		["scale","ordinal"]

            onEnabledChanged: if (!enabled && count > 0) itemDoubleClicked(0)
        }

        AssignedVariablesList
        {
            name:				"covariates"
            title:				qsTr("Covariates")
            allowedColumns:		["ordinal", "scale"]
        }

        AssignedVariablesList
        {
            name:				"factors"
            title:				qsTr("Factors")
            allowedColumns:		["ordinal", "nominal", "nominalText"]
        }

        AssignedVariablesList
        {
            name:              "weights"
            title:             qsTr("Weights (optional)")
            allowedColumns:    ["scale"]
            singleVariable:    true
        }
    }

    Group
    {

        DropDown
        {
            name:				"family"
            label:				qsTr("Family")
            id:					family
            indexDefaultValue:	0
            values:
            [
                { label: qsTr("Binomial"),				value: "binomial"},
                { label: qsTr("Binomial (aggregated)"),	value: "binomialAgg"},
                { label: qsTr("Gaussian"),				value: "gaussian"},
                { label: qsTr("Gamma"),					value: "Gamma"},
                { label: qsTr("Inverse Gaussian"),		value: "inverse.gaussian"},
                { label: qsTr("Poisson"),				value: "poisson"}
            ]

            property var familyMap:
            {
                "binomial":			["logit", "probit", "cauchit", "cloglog"],
                "binomialAgg":      ["logit", "probit", "cauchit", "cloglog"],
                "gaussian":			["identity", "log", "inverse"],
                "Gamma":			["identity", "log", "inverse"],
                "inverse.gaussian":	["identity", "log", "inverse", "1/mu^2"],
                "poisson":			["identity", "log", "sqrt"]
            }

            property var familyDefault:
            {
                "binomial":			"logit",
                "binomialAgg":		"logit",
                "gaussian":			"identity",
                "Gamma":			"inverse",
                "inverse.gaussian":	"1/mu^2",
                "poisson":			"log"
            }

            onCurrentValueChanged:
            {
                if (!familyMap[currentValue].includes(link.value))
                {
                    for (var i = 0; i < link.buttons.length; i++)
                        if (familyMap[currentValue].includes(link.buttons[i].parent.value))
                        {
                            link.buttons[i].parent.click()
                            break;
                        }
                }
            }
        }

        RadioButtonGroup
        {
            id:						link
            name:					"link"
            title:					qsTr("Link")
            radioButtonsOnSameRow:	true

            RadioButton
            {
                label:		qsTr("Logit")
                value:		"logit"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "logit"
            }

            RadioButton
            {
                label:		qsTr("Probit")
                value:		"probit"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "probit"
            }

            RadioButton
            {
                label:		qsTr("Cauchit")
                value:		"cauchit"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "cauchit"
            }

            RadioButton
            {
                label:		qsTr("Complementary LogLog")
                value:		"cloglog"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "cloglog"
            }

            RadioButton
            {
                label:		qsTr("Identity")
                value:		"identity"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "identity"
            }

            RadioButton
            {
                label:		qsTr("Log")
                value:		"log"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "log"
            }

            RadioButton
            {
                label:		qsTr("Sqrt")
                value:		"sqrt"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "sqrt"
            }

            RadioButton
            {
                label:		qsTr("Inverse")
                value:		"inverse"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "inverse"
            }

            RadioButton
            {
                label:		qsTr("1/mu^2")
                value:		"1/mu^2"
                visible:	family.familyMap[family.currentValue].includes(value)
                checked:	family.familyDefault[family.currentValue] == "1/mu^2"
            }
        }
    }

    Section
    {
        title: qsTr("Model")

        VariablesForm
        {
            preferredHeight: jaspTheme.smallDefaultVariablesFormHeight

            AvailableVariablesList
            {
                name: "availableTerms"
                title: qsTr("Components")
                source: ['covariates', 'factors']
                width: parent.width / 4
            }
            ModelTermsList { width: parent.width * 5 / 9 }
        }

        CheckBox { name: "includeIntercept"; label: qsTr("Include intercept"); checked: true }
    }

    Section
    {
        title: qsTr("Statistics")

        Group
        {
            title: qsTr("Parameter Estimates")
            CheckBox { name: "coefEstimates"; label: qsTr("Estimates"); checked: true }
        }
    }
}
