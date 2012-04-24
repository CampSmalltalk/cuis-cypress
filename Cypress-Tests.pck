'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 24 April 2012 at 1:05:22 am'!
'Description Please enter a description for this package '!
!classDefinition: #CypressAbstractTest category: #'Cypress-Tests'!
TestCase subclass: #CypressAbstractTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!
!classDefinition: 'CypressAbstractTest class' category: #'Cypress-Tests'!
CypressAbstractTest class
	instanceVariableNames: ''!

!classDefinition: #CypressDefinitionTest category: #'Cypress-Tests'!
CypressAbstractTest subclass: #CypressDefinitionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!
!classDefinition: 'CypressDefinitionTest class' category: #'Cypress-Tests'!
CypressDefinitionTest class
	instanceVariableNames: ''!

!classDefinition: #CypressLoaderTest category: #'Cypress-Tests'!
CypressAbstractTest subclass: #CypressLoaderTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!
!classDefinition: 'CypressLoaderTest class' category: #'Cypress-Tests'!
CypressLoaderTest class
	instanceVariableNames: ''!

!classDefinition: #CypressPatchTest category: #'Cypress-Tests'!
CypressAbstractTest subclass: #CypressPatchTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!
!classDefinition: 'CypressPatchTest class' category: #'Cypress-Tests'!
CypressPatchTest class
	instanceVariableNames: ''!

!classDefinition: #CypressSnapshotTest category: #'Cypress-Tests'!
CypressAbstractTest subclass: #CypressSnapshotTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!
!classDefinition: 'CypressSnapshotTest class' category: #'Cypress-Tests'!
CypressSnapshotTest class
	instanceVariableNames: ''!

!classDefinition: #CypressStructureTest category: #'Cypress-Tests'!
CypressAbstractTest subclass: #CypressStructureTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Tests'!
!classDefinition: 'CypressStructureTest class' category: #'Cypress-Tests'!
CypressStructureTest class
	instanceVariableNames: ''!


!CypressAbstractTest methodsFor: 'private' stamp: 'dkh 4/24/2012 01:03'!
baseDefinitions
	| className |
	className := 'CypressMockBasic'.
	^{
		(CypressClassDefinition
        		name: className
       		 	superclassName: 'Object'
       			category: 'Cypress-Mocks'
                       	instVarNames: #('name')
			classInstVarNames: #('current')
        		comment:  'This mock contains basic class and instance method selectors').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'extra'
        		category: 'accessing'
        		source:'extra
	"extra method"').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'initialize'
        		category: 'initialization'
        		source:'initialize
	super initialize.
	self name: ''Unknown''').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'name'
        		category: 'accessing'
        		source:'name
	^name').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'name:'
        		category: 'accessing'
        		source:'name: aString
	name := aString').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: true
        		selector: 'current'
        		category: 'accessing'
        		source:'current
	^current').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: true
        		selector: 'current:'
        		category: 'accessing'
        		source:'current: anObject
	current := anObject').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: true
        		selector: 'initialize'
        		category: 'initialization'
        		source:'initialize
	self current: self new').
   	 	(CypressMethodDefinition
          		className: 'Object'
        		classIsMeta: false
        		selector: 'isCypressMockBasic'
        		category: '*Cypress-Mocks'
        		source:'isCypressMockBasic
	^false')
	}
! !

!CypressAbstractTest methodsFor: 'private' stamp: 'dkh 4/24/2012 00:45'!
basePackageStructureJson
    ^ '{
	"name" : "Cypress-Mocks.package",
	"contents" : [
		{
			"name" : "CypressMockBasic.class",
			"instance" : [
				{
					"name" : "extra.st",
					"contents" : "accessing%0Aextra%0A%09%22extra%20method%22"
				 },
				{
					"name" : "initialize.st",
					"contents" : "initialization%0Ainitialize%0A%09super%20initialize.%0A%09self%20name%3A%20%27Unknown%27"
				 },
				{
					"name" : "name.st",
					"contents" : "accessing%0Aname%0A%09%5Ename"
				 },
				{
					"name" : "name..st",
					"contents" : "accessing%0Aname%3A%20aString%0A%09name%20%3A%3D%20aString"
				 }			],
			"class" : [
				{
					"name" : "current.st",
					"contents" : "accessing%0Acurrent%0A%09%5Ecurrent"
				 },
				{
					"name" : "current..st",
					"contents" : "accessing%0Acurrent%3A%20anObject%0A%09current%20%3A%3D%20anObject"
				 },
				{
					"name" : "initialize.st",
					"contents" : "initialization%0Ainitialize%0A%09self%20current%3A%20self%20new"
				 }			],
			"README.md" : "This%20mock%20contains%20basic%20class%20and%20instance%20method%20selectors",
			"properties.json" : {
				"classinstvars" : [
					"current" ],
				"instvars" : [
					"name" ],
				"name" : "CypressMockBasic",
				"super" : "Object" }
		 },
		{
			"name" : "Object.extension",
			"instance" : [
				{
					"name" : "isCypressMockBasic.st",
					"contents" : "%2ACypress-Mocks%0AisCypressMockBasic%0A%09%5Efalse"
				 }			],
			"class" : [
			],
			"properties.json" : {
				"name" : "Object" }
		 }
	],
	"properties.json" : {
		 }
}'! !

!CypressAbstractTest methodsFor: 'private'!
baseTargetPatch
	| className |
	className := 'CypressMockBasic'.
	^{
		(CypressAddition 
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'added'
        			category: 'accessing'
        			source:'added
	"added method"')).
		(CypressModification 
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'name:'
        			category: 'accessing'
        			source:'name: aString
	name := aString') 
			to: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'name:'
        			category: 'accessing'
        			source:'name: aString
	"changed method"
	name := aString')).
		(CypressRemoval 
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'extra'
        			category: 'accessing'
        			source:'extra
	"extra method"')).
		(CypressRemoval 
			of: (CypressMethodDefinition
          			className: 'Object'
        			classIsMeta: false
        			selector: 'isCypressMockBasic'
        			category: '*Cypress-Mocks'
        			source:'isCypressMockBasic
	^false'))
	}
! !

!CypressAbstractTest methodsFor: 'private' stamp: 'dkh 4/24/2012 01:04'!
classComment

	^'This mock contains basic class and instance method selectors'
! !

!CypressAbstractTest methodsFor: 'private' stamp: 'dkh 4/23/2012 21:33'!
compileJSON: aJsonString

	^CypressJsonParser parse: aJsonString! !

!CypressAbstractTest methodsFor: 'private' stamp: 'dkh 4/24/2012 00:04'!
sampleJson

	^'{
	"age" : 25,
	"name" : "John%20Smith",
	"phoneNumber" : [
		{
			"number" : "212%20555-1234",
			"type" : "home" },
		{
			"number" : "646%20555-4567",
			"type" : "fax" } ],
	"registered" : true }'
! !

!CypressAbstractTest methodsFor: 'private'!
targetDefinitions
	"remove #extra method and modify #name: method"

	| className |
	className := 'CypressMockBasic'.
	^{
		(CypressClassDefinition
        		name: className
       		 	superclassName: 'Object'
       			category: 'Cypress-Mocks'
                       	instVarNames: #('name')
			classInstVarNames: #('current')
        		comment: self classComment).
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'added'
        		category: 'accessing'
        		source:'added
	"added method"').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'initialize'
        		category: 'initialization'
        		source:'initialize
	super initialize.
	self name: ''Unknown''').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'name'
        		category: 'accessing'
        		source:'name
	^name').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: false
        		selector: 'name:'
        		category: 'accessing'
        		source:'name: aString
	"changed method"
	name := aString').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: true
        		selector: 'current'
        		category: 'accessing'
        		source:'current
	^current').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: true
        		selector: 'current:'
        		category: 'accessing'
        		source:'current: anObject
	current := anObject').
   	 	(CypressMethodDefinition
          		className: className
        		classIsMeta: true
        		selector: 'initialize'
        		category: 'initialization'
        		source:'initialize
	self current: self new')
	}
! !

!CypressAbstractTest methodsFor: 'private' stamp: 'dkh 4/24/2012 00:55'!
validatePackage: package against: expectedDefinitions

	| packageDefinitions cd1 cd2 |
	packageDefinitions := package snapshot definitions.
	self assert: (expectedDefinitions size = packageDefinitions size).
	cd1 := packageDefinitions detect: [:each | each isKindOf: CypressClassDefinition].
	cd2 :=  expectedDefinitions detect: [:each | each isKindOf: CypressClassDefinition].
	self assert: cd1 = cd2.
	packageDefinitions do: [:def |
		(expectedDefinitions includes: def)
			ifFalse: [ 
				def inspect.
				self assert: false ]].
! !

!CypressDefinitionTest methodsFor: 'testing'!
testClassDefinition
	self assert: (CypressClassDefinition
		name: 'Foo'
       		 superclassName: 'Object'
       		category: 'Foo'
                instVarNames: #()
		classInstVarNames: #()
        	comment: '') printString =  'a CypressClassDefinition (Foo)'
! !

!CypressDefinitionTest methodsFor: 'testing'!
testDictionaryOfDefinitions

	| dict |
	"baseDefinitions"
	dict := Dictionary new.
	self baseDefinitions do: [:each | 
		dict at: each put: each ].
	self baseDefinitions do: [:each | 
		self assert: (dict at: each) = each ].

	"targetDefinitions"
	dict := Dictionary new.
	self targetDefinitions do: [:each | 
		dict at: each put: each ].
	self targetDefinitions do: [:each | 
		self assert: (dict at: each) = each ].
! !

!CypressDefinitionTest methodsFor: 'testing'!
testEquality
	| pkg1 pkg2 pkg3 name |
	name := 'Cypress-Mocks'.
	pkg1 := CypressPackageDefinition new name: name.
	pkg2 := CypressPackageDefinition new name: name.
	pkg3 := CypressPackageDefinition new name: 'Nope!!'.

	self assert: pkg1 equals: pkg2.
	self deny: pkg1 = pkg3
! !

!CypressDefinitionTest methodsFor: 'testing'!
testMethodDefinition
	self assert: (CypressMethodDefinition
		className: 'Foo'
		classIsMeta: false
		selector: 'isFoo'
		category: 'testing'
		source: 'isFoo ^true') printString = 'a CypressMethodDefinition (Foo>>isFoo)'
! !

!CypressDefinitionTest methodsFor: 'testing'!
testNameEquality
	| pkg name |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition new name: name.
	self assert: pkg name equals: name.
	self deny: (pkg name = 'Nope.').
! !

!CypressDefinitionTest methodsFor: 'testing'!
testPrintString
	| name pkg |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition new name: name.
	self assert: 'a CypressPackageDefinition(', name, ')' equals: pkg printString.
! !

!CypressLoaderTest methodsFor: 'running'!
tearDown

	| name |
	super tearDown.
	name := 'Cypress-Mocks'.
	(CypressSnapshot definitions: self baseDefinitions)
		 updatePackage: (CypressPackageDefinition new name: name)
! !

!CypressLoaderTest methodsFor: 'testing'!
testLoad

	| name |
	name := 'Cypress-Mocks'.
	(CypressSnapshot definitions: self targetDefinitions)
		 updatePackage: (CypressPackageDefinition new name: name)
! !

!CypressPatchTest methodsFor: 'testing'!
testDictionaryOfPatchOperations
	"loader uses dictionary for managing patch operations ... ensure that Amber Dictionaries stand up"

	| dict |
	dict := Dictionary new.
	self baseTargetPatch do: [:each | 
		dict at: each put: each ].
	self baseTargetPatch do: [:each | 
		self assert: (dict at: each) = each ].
! !

!CypressPatchTest methodsFor: 'testing' stamp: 'dkh 4/24/2012 00:18'!
testPatch
    | baseSnapshot targetSnapshot patch operations expected |
    baseSnapshot := CypressSnapshot definitions: self baseDefinitions.
    targetSnapshot := CypressSnapshot definitions: self targetDefinitions.
    patch := CypressPatch fromBase: baseSnapshot toTarget: targetSnapshot.
    operations := patch operations.
    self assert: operations size = 4.
    expected := self baseTargetPatch asArray.
    1 to: operations size do: [ :index | 
        | op |
        op := operations at: index.
        self assert: (expected includes: op) ]! !

!CypressPatchTest methodsFor: 'testing'!
testPatchOperationEquality

	| className modification removal addition |
	className := 'CypressMockBasic'.
	modification := CypressModification 
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'name:'
        			category: 'accessing'
        			source:'name: aString
	name := aString') 
			to: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'name:'
        			category: 'accessing'
        			source:'name: aString
	"changed method"
	name := aString').
	self assert: modification = modification.
	removal := CypressRemoval 
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'extra'
        			category: 'accessing'
        			source:'extra
	"extra method"').
	self assert: removal = removal.
	addition := CypressAddition
			of: (CypressMethodDefinition
          			className: className
        			classIsMeta: false
        			selector: 'extra'
        			category: 'accessing'
        			source:'extra
	"extra method"').
	self assert: addition = addition.
! !

!CypressSnapshotTest methodsFor: 'testing'!
testSnapshot
	| name pkg  |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition new name: name.
	self validatePackage: pkg against: self baseDefinitions
! !

!CypressSnapshotTest methodsFor: 'testing' stamp: 'dkh 4/24/2012 00:20'!
testSnapshotEquality
	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition new name: name.
	packageDefinitions := pkg snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: packageDefinitions asArray = expectedDefinitions asArray
! !

!CypressStructureTest methodsFor: 'tests' stamp: 'dkh 4/24/2012 00:59'!
testClassStructure

	| jsObject packageStructure classStructure classProperties |
	jsObject := self compileJSON: self basePackageStructureJson.
	packageStructure := CypressPackageStructure fromJs: jsObject.
	classStructure := packageStructure classes first.
	self assert: classStructure name = 'CypressMockBasic'.
	self deny: classStructure isClassExtension.
	self assert: classStructure comment =  'This mock contains basic class and instance method selectors'..
	classProperties := classStructure properties.
	self assert: classProperties size = 4.
	self assert: (classProperties at: 'instvars') = #('name').
	self assert: (classProperties at: 'classinstvars') = #('current').
	self assert: (classProperties at: 'name') = 'CypressMockBasic'.
	self assert: (classProperties at: 'super') = 'Object'.
	self assert: classStructure instanceMethods size = 4.
	self assert: classStructure classMethods size = 3.
	classStructure := packageStructure extensions first.
	self assert: classStructure name = 'Object'.
	self assert: classStructure isClassExtension.
	self assert: classStructure comment = ''.
	classProperties := classStructure properties.
	self assert: classProperties size = 1.
	self assert: (classProperties at: 'name') = 'Object'.
	self assert: classStructure instanceMethods size = 1.
	self assert: classStructure classMethods size = 0.
! !

!CypressStructureTest methodsFor: 'tests'!
testJson
	"Let's compile the JSON without errors"

	self compileJSON: self basePackageStructureJson
! !

!CypressStructureTest methodsFor: 'tests'!
testPackageStructureFromJson

	| packageStructure classStructure classProperties |
	packageStructure := CypressPackageStructure fromJson: self basePackageStructureJson.
	self assert: packageStructure name = 'Cypress-Mocks.package'.
	self assert: packageStructure packageName = 'Cypress-Mocks'.
	self assert: packageStructure properties isEmpty.
	self assert: packageStructure extensions size = 1.
	self assert: packageStructure classes size = 1.
! !

!CypressStructureTest methodsFor: 'tests'!
testPackageStructureFromPackage

	| packageStructure |
	packageStructure := CypressPackageStructure fromPackage: (CypressPackageDefinition new name: 'Cypress-Mocks').
	self validatePackage: packageStructure against: self baseDefinitions
! !

!CypressStructureTest methodsFor: 'tests'!
testPackageStructureSnapshot

	| packageStructure |
	packageStructure := CypressPackageStructure fromJs: (self compileJSON: self basePackageStructureJson).
	self validatePackage: packageStructure against: self baseDefinitions
! !

!CypressStructureTest methodsFor: 'tests' stamp: 'dkh 4/24/2012 00:38'!
testPackageStructureToJson

	| packageStructure stream json |
	packageStructure := CypressPackageStructure fromPackage: (CypressPackageDefinition new name: 'Cypress-Mocks').
	stream := WriteStream on: String new.
	packageStructure writeJsonOn: stream.
	json := stream contents.
	self assert: (self basePackageStructureJson withLineEndings: String lfString) = (json withLineEndings: String lfString)
! !

!CypressStructureTest methodsFor: 'tests'!
testPropertyDictionaryRead

	| propertyDictionary phoneNumbers |
	propertyDictionary := (self compileJSON: self sampleJson) asCypressPropertyObject.
	self assert: (propertyDictionary at: 'name') = 'John Smith'.
	self assert: (propertyDictionary at: 'age') = 25.
	self assert: (propertyDictionary at: 'registered').
	phoneNumbers := propertyDictionary at: 'phoneNumber'.
	self assert: phoneNumbers size = 2.
	self assert: ((phoneNumbers at: 1) at: 'number') = '212 555-1234'.
	self assert: ((phoneNumbers at: 2) at: 'number') = '646 555-4567'.
! !

!CypressStructureTest methodsFor: 'tests' stamp: 'dkh 4/23/2012 23:59'!
testPropertyDictionaryWrite

	| propertyDictionary stream x y |
	propertyDictionary := (self compileJSON: self sampleJson) asCypressPropertyObject.
	stream := WriteStream on: String new.
	propertyDictionary writeCypressJsonOn: stream indent: 0.
	self assert: (x:= stream contents withLineEndings: String lfString)  = (y := self sampleJson withLineEndings: String lfString)
! !
