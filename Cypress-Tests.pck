'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 24 April 2012 at 12:36:15 am'!
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


!CypressAbstractTest methodsFor: 'private'!
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
        		comment: self classComment).
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

!CypressAbstractTest methodsFor: 'private'!
basePackageStructureJson

	^'{
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
			"README.md" : "%23%23%20Class%20Comment%0A%0AThis%20mock%20contains%20basic%20class%20and%20instance%20method%20selectors.%0A%0A%20%5B**GitHub%20Flabored%20Markdown**%5D%5B1%5D%20with%20**Smalltalk**%20sytax%20*highlighting*%3A%0A%0A%60%60%60Smalltalk%0Ainitialize%0A%09super%20initialize.%0A%09self%20name%3A%20%27Unknown%27%0A%60%60%60%0A%0AAnd%20some%20%5BUTF8%20samples%5D%5B2%5D%3A%20%0A%0A%60%60%60%0A%20%20Lithuanian%3A%20A%u0161%20galiu%20valgyti%20stikl%u0105%20ir%20jis%20man%u0119s%20ne%u017Eeid%u017Eia%0A%20%20Russian%3A%20%u042F%20%u043C%u043E%u0433%u0443%20%u0435%u0441%u0442%u044C%20%u0441%u0442%u0435%u043A%u043B%u043E%2C%20%u043E%u043D%u043E%20%u043C%u043D%u0435%20%u043D%u0435%20%u0432%u0440%u0435%u0434%u0438%u0442.%0A%20%20Korean%3A%20%uB098%uB294%20%uC720%uB9AC%uB97C%20%uBA39%uC744%20%uC218%20%uC788%uC5B4%uC694.%20%uADF8%uB798%uB3C4%20%uC544%uD504%uC9C0%20%uC54A%uC544%uC694%0A%20%20Hebrew%3A%20%u05D0%u05E0%u05D9%20%u05D9%u05DB%u05D5%u05DC%20%u05DC%u05D0%u05DB%u05D5%u05DC%20%u05D6%u05DB%u05D5%u05DB%u05D9%u05EA%20%u05D5%u05D6%u05D4%20%u05DC%u05D0%20%u05DE%u05D6%u05D9%u05E7%20%u05DC%u05D9.%0A%20%20Latin%3A%20Vitrum%20edere%20possum%3B%20mihi%20non%20nocet.%0A%60%60%60%0A%0A%0A%5B1%5D%3A%20http%3A//github.github.com/github-flavored-markdown/%0A%5B2%5D%3A%20http%3A//www.columbia.edu/%7Efdc/utf8/",
			"properties.json" : {
				"name" : "CypressMockBasic",
				"super" : "Object",
				"instvars" : [
					"name" ],
				"classinstvars" : [
					"current" ] }
		 },
		{
			"name" : "Object.extension",
			"instance" : [
				{
					"name" : "isCypressMockBasic.st",
					"contents" : "*Cypress-Mocks%0AisCypressMockBasic%0A%09%5Efalse"
				 }			],
			"class" : [
			],
			"properties.json" : {
				"name" : "Object" }
		 }
	],
	"properties.json" : {
		 }
}'
! !

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

!CypressAbstractTest methodsFor: 'private' stamp: 'dkh 4/24/2012 00:15'!
classComment

	^'## Class Comment

This mock contains basic class and instance method selectors.

 [**GitHub Flabored Markdown**][1] with **Smalltalk** sytax *highlighting*:

```Smalltalk
initialize
	super initialize.
	self name: ''Unknown''
```

And some [UTF8 samples][2]: 

```
UTF8 not handled correctly
```


[1]: http://github.github.com/github-flavored-markdown/
[2]: http://www.columbia.edu/~fdc/utf8/'
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

!CypressAbstractTest methodsFor: 'private'!
validatePackage: package against: expectedDefinitions

	| packageDefinitions |
	packageDefinitions := package snapshot definitions.
	self assert: (expectedDefinitions size = packageDefinitions size).
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

!CypressStructureTest methodsFor: 'tests'!
testClassStructure

	| jsObject packageStructure classStructure classProperties |
	jsObject := self compileJSON: self basePackageStructureJson.
	packageStructure := CypressPackageStructure fromJs: jsObject.
	classStructure := packageStructure classes first.
	self assert: classStructure name = 'CypressMockBasic'.
	self deny: classStructure isClassExtension.
	self assert: classStructure comment = self classComment.
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

!CypressStructureTest methodsFor: 'tests' stamp: 'dkh 4/23/2012 20:25'!
testPackageStructureToJson

	| packageStructure stream json |
	packageStructure := CypressPackageStructure fromPackage: (CypressPackageDefinition new name: 'Cypress-Mocks').
	stream := WriteStream on: String new.
	packageStructure writeJsonOn: stream.
	json := stream contents.
	self assert: self basePackageStructureJson = json
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
