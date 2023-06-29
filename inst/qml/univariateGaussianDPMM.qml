
import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls
import JASP.Widgets
import JASP.Theme

Form {
	id: form


	Formula { rhs: "dependent" }


		

		Group
		{
			title:		qsTr("Variable selection") 
			id:	variableOptions
	VariablesForm{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "dependent"; 
								title: qsTr("Dependent Variable"); 
								suggestedColumns: ["scale"] ; 
								singleVariable:	true}
	}

	CheckBox
		{
			name: "dependentScaled"
			label: qsTr("Scale dependent variable (recommended to speed up convergence)")
			checked: true
		}

		CheckBox 
			{
				name:							"setSeed"
				text:							qsTr("Set seed")
				childrenOnSameRow:				true
				checked: true

				IntegerField 
				{
					name:						"seed"
					defaultValue:				123
					min:						-999999
					max:						999999
					fieldWidth:					60
				}
			}
		}
	
	
		
Section{
	title: qsTr("Prior")
		Group
		{
			title:		qsTr("Prior distribution (Inverse-Gamma distribution)") 
			id:	prioroptions

		DoubleField
		{
			name:			"muPrior"
			label:			qsTr("Mu")
			defaultValue:	0
			min:			-1000000000

		}

		IntegerField
		{
			name:			"kPrior"
			label:			qsTr("K")
			defaultValue:	1
			min:			1
		}
		
		DoubleField
		{
			name:			"alphaPrior"
			label:			qsTr("Alpha (Shape)")
			defaultValue:	1
			min:			0.1
			decimals:		1
		}
		
		DoubleField
		{
			name:			"betaPrior"
			label:			qsTr("Beta (Scale)")
			defaultValue:	1
			min:			0.1
			decimals:		1
		}
		
		CheckBox{
		name: "priorPlot"
		label:			qsTr("Plot Prior distribution")
		checked: true
	}
		}
	
}




		 
		Section{
				title: qsTr("Sampler options")
		Group
		{
			title:		qsTr("Sampler options")
			id:	sampleroptions
	IntegerField
		{
			name:			"mcmcBurnin"
			id:				warmup
			label:			qsTr("Burnin")
			defaultValue:	100
			min:			30
		}
	IntegerField
		{
			name:			"mcmcSamples"
			label:			qsTr("Samples")
			defaultValue:	200
			min:			parseInt(warmup.value) + 100
		}		
	
	}
		}

	Section{
		title: qsTr("Hyper-parameters options")
		id: hyperoptions

		
	DoubleField
		{
			name:			"alpha"
			label:			qsTr("Alpha")
			defaultValue:	1
			min: 			0.1
			decimals:		1
		}

		IntegerField
		{
			name:			"kluster"
			label:			qsTr("kluster")
			defaultValue:	1
			min:			1
		}
		
	
	}

	Section{
		title: qsTr("Plots and tables")
CheckBox
		{
			name: "tracePlots"
			label: qsTr("Trace plot (convergence diagnostics)")
			checked: true
		}
CheckBox
		{
			name: "priorPosteriorPlot"
			label: qsTr("Prior and Posterior plot")
			checked: true
		}
CheckBox
		{
			name: "clusterDensityPlot"
			label: qsTr("Plot density clusters")
			checked: true
		}
		
CheckBox
{
name: "tableCluster";		label: qsTr("Table of cluster means and standard deviations") ;checked: true
CheckBox { name: "clusterAdditionalInfo";		label: qsTr("Additional info"); checked: true }
CIField  { name: "clusterCiLevel";	label: qsTr("Credible interval") }
}
		
		
	}

	Section{
	title:							qsTr("Export Results")

	CheckBox 
	{
		id:							addPredictions
		name:						"addPredictions"
		text:						qsTr("Add predictions to data")
		checked: true

		ComputedColumnField 
		{
			id:						predictionsColumn
			name:					"predictionsColumn"
			text:					qsTr("Column name")
			placeholderText:		qsTr("e.g., cluster")
			fieldWidth:				120
			enabled:				addPredictions.checked
		}
	}
	
Group{
	FileSelector
		{
			id:						savePath
			name:					"savePath"
			label:					qsTr("Save as")
			placeholderText:		qsTr("e.g., location/model.jaspML")
			filter:					"*.jaspML"
			save:					true
			fieldWidth:				180 * preferencesModel.uiScale
		}

		CheckBox
		{
			id:						saveModel
			name:					"saveModel"
			text:					qsTr("Save trained model")
			enabled:				showSave && savePath.value != ""
			Layout.leftMargin:		10 * preferencesModel.uiScale
		}
}
		}
}


