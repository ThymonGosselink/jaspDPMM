
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
			id:	variableoptions
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
			name: "scaledependent"
			label: qsTr("Scale dependent variable (recommended to speed up convergence)")
			checked: true
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
			name:			"mu0"
			label:			qsTr("mu0")
			defaultValue:	0
			min:			-1000000000

		}

		IntegerField
		{
			name:			"k0"
			label:			qsTr("k0")
			defaultValue:	1
			min:			1
		}
		
		DoubleField
		{
			name:			"alpha0"
			label:			qsTr("alpha0 (Shape)")
			defaultValue:	1
			min:			0.1
			decimals:		1
		}
		
		DoubleField
		{
			name:			"beta0"
			label:			qsTr("beta0 (Scale)")
			defaultValue:	1
			min:			0.1
			decimals:		1
		}
		
		CheckBox{
		name: "plotprior"
		label:			qsTr("plot distribution")
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
			label:			qsTr("amount of clusters at the start")
			defaultValue:	1
			min:			1
		}
		
	
	}

	Section{
		title: qsTr("Plots and tables")
CheckBox
		{
			name: "traceplots"
			label: qsTr("Trace plot (convergence diagnostics)")
			checked: true
		}
CheckBox
		{
			name: "priorposteriorplot"
			label: qsTr("Prior and Posterior plot")
			checked: true
		}
CheckBox
		{
			name: "clusterdensityplot"
			label: qsTr("Plot density clusters")
			checked: true
		}
CheckBox
		{
			name: "tablecluster"
			label: qsTr(" Table of cluster means and standard deviations")
			checked: true
		}
		
	}
		}
	


