Smalltalk current createPackage: 'Cypress-ExportImport' properties: #{}!
AmberProjectExporter subclass: #CypressAmberExporter
	instanceVariableNames: ''
	package: 'Cypress-ExportImport'!

!CypressAmberExporter methodsFor: 'exporting'!

export
	self exportPackageStructure
!

exportPackageStructure
	|  packageStructure stream |
	packageStructure := CypressPackageStructure fromPackage: (CypressPackageDefinition new name: self packageName).
	stream := StringStream on: String new.
	packageStructure writeJsonOn: stream.
	self 
		ajaxPutAt: 'CYPRESS_PACKAGE/', self prefix
		data: stream contents
! !

!CypressAmberExporter class methodsFor: 'initialization'!

initialize

	(AmberProjectExporter exporters)
		 at: 'cypress' put: self
! !

AmberProjectImporter subclass: #CypressAmberImporter
	instanceVariableNames: ''
	package: 'Cypress-ExportImport'!

!CypressAmberImporter class methodsFor: 'importing'!

importCypressPackages: packageNameList prefix: prefix
	"CypressAmberImporter
		importCypressPackages: #('Cypress-Mocks')
		prefix:''"

	self 
		import: #cypress
		subDir: '' 
		packages: packageNameList 
		extension: '.package' 
		prefix: prefix
! !

!CypressAmberImporter class methodsFor: 'private'!

get: type url: ajaxUrl onSuccess: successBlock
	
	type == #cypress ifTrue: [ ^self  getCypress: ajaxUrl onSuccess: successBlock ].
	super get: type url: ajaxUrl onSuccess: successBlock
!

getCypress: ajaxUrl onSuccess: successBlock

	jQuery 
		ajax: 'CYPRESS_PACKAGE/', ajaxUrl
        	options: #{
			'type' -> 'GET'.
			'async' -> false.
    			'complete' -> [:jqXHR :textStatus | 
			jqXHR readyState = 4 ifTrue: [ | packageStructure |
				packageStructure := CypressPackageStructure fromJson: jqXHR responseText.
				packageStructure snapshot
		 			updatePackage: (CypressPackageDefinition new name: packageStructure packageName).
				successBlock value  ]].
 			'error' -> [window alert: 'getCypress request failed at:  ', ajaxUrl]
		}
! !

