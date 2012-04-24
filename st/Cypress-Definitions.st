Smalltalk current createPackage: 'Cypress-Definitions' properties: #{}!
Object subclass: #CypressDefinition
	instanceVariableNames: ''
	package: 'Cypress-Definitions'!

!CypressDefinition methodsFor: 'accessing'!

description
	self subclassResponsibility
! !

!CypressDefinition methodsFor: 'comparing'!

= aDefinition
	^(aDefinition isKindOf: CypressDefinition) and: [self isRevisionOf: aDefinition]
! !

!CypressDefinition methodsFor: 'dependency'!

provisions
	"Answer list of global names defined by this definition"

	^#()
!

requirements
	"Answer list of global names required by this definition"

	^#()
! !

!CypressDefinition methodsFor: 'loading'!

actualClass

	self subclassResponsibility
!

loadClassDefinition
	"default is to do nothing"
!

loadMethodDefinition
	"default is to do nothing"
!

postLoad
	"noop"
!

postLoadOver: aDefinition

	self postLoad
!

unloadDefinition

	self subclassResponsibility
! !

!CypressDefinition methodsFor: 'testing'!

isRevisionOf: aDefinition
	^ (aDefinition isKindOf: CypressDefinition) and: [aDefinition description = self description]
!

isSameRevisionAs: aDefinition
	^ self = aDefinition
! !

!CypressDefinition methodsFor: 'visiting'!

classDefinition: classBlock methodDefinition: methodBlock
	"default is noop"
! !

CypressDefinition subclass: #CypressClassDefinition
	instanceVariableNames: 'name superclassName category comment instVarNames classInstVarNames'
	package: 'Cypress-Definitions'!

!CypressClassDefinition methodsFor: 'accessing'!

category

	^category
!

classInstVarNames

	^classInstVarNames
!

className

	^self name
!

comment

	^comment
!

description

	^ Array with: name
!

instVarNames

	^instVarNames
!

name

	^name
!

superclassName

	^superclassName
! !

!CypressClassDefinition methodsFor: 'comparing'!

= aDefinition
	^(super = aDefinition)
		and: [superclassName = aDefinition superclassName
		and: [category = aDefinition category
		and: [instVarNames = aDefinition instVarNames
		and: [classInstVarNames = aDefinition classInstVarNames
		and: [comment = aDefinition comment]]]]]
! !

!CypressClassDefinition methodsFor: 'converting'!

asCypressClassDefinition

	^self
! !

!CypressClassDefinition methodsFor: 'dependency'!

provisions
	"Answer list of global names defined by this definition"

	^{ self name }
!

requirements
	"Answer list of global names required by this definition"

	^{self name}
! !

!CypressClassDefinition methodsFor: 'initialization'!

name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: anInstanceVariableNames classInstVarNames: aClassInstanceVariableNames comment: aComment

	name := aClassName.
	superclassName := aSuperclassName.
	category := aCategory.
	instVarNames := anInstanceVariableNames.
	classInstVarNames := aClassInstanceVariableNames.
	comment := aComment
! !

!CypressClassDefinition methodsFor: 'loading'!

actualClass

	^Smalltalk current at: self name
!

createClass

	| superClass |
	superClass := Smalltalk current at: self superclassName.
	^ClassBuilder new
		superclass: superClass 
		subclass: self name
		instanceVariableNames: (self stringForVariables: self instVarNames)
		package: self category
!

loadClassDefinition

	 | cls |
	cls := self createClass.
	cls class instanceVariableNames: (self stringForVariables: self classInstVarNames).
	self comment notEmpty ifTrue: [ cls comment: self comment ]
!

unloadDefinition

	Smalltalk current removeClass: self actualClass.
! !

!CypressClassDefinition methodsFor: 'printString'!

printString

	| str |
	str := StringStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self name;
		nextPutAll: ')'.
	^str contents
!

stringForVariables: aCollectionOfVariableNames
	^ String streamContents:
		[:stream |
		aCollectionOfVariableNames
			do: [:ea | stream nextPutAll: ea]
			separatedBy: [stream space]]
! !

!CypressClassDefinition methodsFor: 'visiting'!

classDefinition: classBlock methodDefinition: methodBlock

	classBlock value: self
! !

!CypressClassDefinition class methodsFor: 'instance creation'!

name: aClassName 
superclassName: aSuperclassName
category: aCategory
instVarNames: anInstanceVariableNames
classInstVarNames: aClassInstanceVariableNames
comment: aComment

	^(self new) 
		name: aClassName 
		superclassName: aSuperclassName
		category: aCategory
		instVarNames: anInstanceVariableNames
		classInstVarNames: aClassInstanceVariableNames
		comment: aComment
! !

CypressDefinition subclass: #CypressMethodDefinition
	instanceVariableNames: 'classIsMeta source category selector className'
	package: 'Cypress-Definitions'!

!CypressMethodDefinition methodsFor: 'accessing'!

category

	^category
!

classIsMeta

	^classIsMeta
!

className

	^className
!

description
	^ Array	
		with: className
		with: selector
		with: classIsMeta
!

selector

	^selector
!

source

	^source
! !

!CypressMethodDefinition methodsFor: 'comparing'!

= aDefinition
    ^ super = aDefinition
        and: [ aDefinition source = self source
                and: [ aDefinition category = self category ] ]
! !

!CypressMethodDefinition methodsFor: 'converting'!

asCypressMethodDefinition

	^self
! !

!CypressMethodDefinition methodsFor: 'dependency'!

requirements
	"Answer list of global names required by this definition"

	^{self className}
! !

!CypressMethodDefinition methodsFor: 'initialization'!

className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource

	className := aName.
	classIsMeta := isMetaclass.
	selector := aSelector.
	category := aCategory.
	source := aSource.
! !

!CypressMethodDefinition methodsFor: 'loading'!

actualClass

	| cls |
	cls := self theNonMetaClass.
	^self classIsMeta
		ifTrue: [ cls class ]
		ifFalse: [ cls  ].
!

loadMethodDefinition

	self actualClass
		compile: self source
		category: self category
!

postLoadOver: aDefinition

	super postLoadOver: aDefinition.
	(self isInitializer
		and: [ aDefinition isNil or: [ self source ~= aDefinition source ]]) 
			ifTrue: [ self theNonMetaClass initialize ].
!

theNonMetaClass
	^Smalltalk current at: self className
!

unloadDefinition

	| cls |
	cls := self actualClass.
	cls removeCompiledMethod: (cls methodAt: self selector)
! !

!CypressMethodDefinition methodsFor: 'printing'!

printString

	| str |
	str := StringStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self className.
	self classIsMeta
		ifTrue: [ str nextPutAll: ' class' ].
	str 
		nextPutAll: '>>';
		nextPutAll: self selector;
		nextPutAll: ')'.
	^str contents
! !

!CypressMethodDefinition methodsFor: 'testing'!

isInitializer
	^ self selector = 'initialize' and: [self classIsMeta]
! !

!CypressMethodDefinition methodsFor: 'visiting'!

classDefinition: classBlock methodDefinition: methodBlock

	methodBlock value: self
!

instanceMethod: instanceBlock classMethod: classBlock

	^(self classIsMeta
		ifTrue: [ classBlock ]
		ifFalse: [ instanceBlock ]) value: self
! !

!CypressMethodDefinition class methodsFor: 'instance creation'!

className: aName
classIsMeta: isMetaclass
selector: aSelector
category: aCategory
source: aSource

	^(self new)
		className: aName
		classIsMeta: isMetaclass
		selector: aSelector
		category: aCategory
		source: aSource
! !

Object subclass: #CypressDefinitionIndex
	instanceVariableNames: 'definitionMap'
	package: 'Cypress-Definitions'!

!CypressDefinitionIndex methodsFor: 'accessing'!

definitionMap
	definitionMap ifNil: [ definitionMap := Dictionary new ].
	^ definitionMap
!

definitions
	^self definitionMap values
! !

!CypressDefinitionIndex methodsFor: 'adding'!

add: aDefinition
	^ self definitionMap at: aDefinition description put: aDefinition
!

addAll: aCollection
	aCollection do: [:ea | self add: ea]
! !

!CypressDefinitionIndex methodsFor: 'querying'!

definitionLike: aDefinition ifPresent: foundBlock ifAbsent: errorBlock
	| definition |
	definition := self definitionMap at: aDefinition description ifAbsent: [].
	^ definition
		ifNil: errorBlock
		ifNotNil: [foundBlock value: definition]
! !

!CypressDefinitionIndex methodsFor: 'removing'!

remove: aDefinition
	self definitionMap removeKey: aDefinition description ifAbsent: []
! !

!CypressDefinitionIndex class methodsFor: 'instance creation'!

definitions: aCollection
	^ self new addAll: aCollection
! !

Object subclass: #CypressDependencySorter
	instanceVariableNames: 'required provided orderedItems'
	package: 'Cypress-Definitions'!

!CypressDependencySorter methodsFor: 'accessing'!

externalRequirements
	| unloaded providedByUnloaded |
	unloaded := self itemsWithMissingRequirements.
	providedByUnloaded := (unloaded gather: [:e | e provisions]) asSet.
	^ self required keys reject: [:globalName | providedByUnloaded includes: globalName ]
!

itemsWithMissingRequirements
	| patchOperations |
	patchOperations := Set new.
	self required values do: [:aSetOfPatchOperations | patchOperations addAll: aSetOfPatchOperations ].
	^ patchOperations
!

orderedItems
	"ordered list of patch operations"

	orderedItems ifNil: [ orderedItems := OrderedCollection new ].
	^orderedItems
!

provided
	"set of global names provided by definitions already loaded"

	provided ifNil: [ provided := Set new ].
	^provided
!

required
	"dictionary of required global name mapped to list of definitions that require the global"

	required ifNil: [ required := Dictionary new ].
	^required
! !

!CypressDependencySorter methodsFor: 'building'!

add: aPatchOperation
	| requirements |
	requirements := self unresolvedRequirementsFor: aPatchOperation.
	requirements isEmpty
		ifTrue: [self addToOrder: aPatchOperation]
		ifFalse: [self addRequirements: requirements for: aPatchOperation].
	^ aPatchOperation
!

addAll: aCollection
	aCollection do: [:aPatchOperation | self add: aPatchOperation ]
! !

!CypressDependencySorter methodsFor: 'private'!

addExternalProvisions: aCollection
	(aCollection intersection: self externalRequirements)
		do: [:globalName | self addProvision: globalName]
!

addProvision: aGlobalName
	| newlySatisfied |
	self provided add: aGlobalName.
	newlySatisfied := self required removeKey: aGlobalName ifAbsent: [#()].
	self addAll: newlySatisfied.
!

addRequirement: globalName for: aPatchOperation
	(self itemsRequiring: globalName) add: aPatchOperation
!

addRequirements: aCollection for: aPatchOperation
	aCollection do: [:globalName | self addRequirement: globalName for: aPatchOperation]
!

addToOrder: aPatchOperation
	self orderedItems add: aPatchOperation.
	aPatchOperation provisions do: [:globalName | self addProvision: globalName ].
!

itemsRequiring: globalName
	^ self required at: globalName ifAbsentPut: [Set new]
!

unresolvedRequirementsFor: aPatchOperation
	"Answer a list of global names that are required by <aPatchOperation>, but not 
	 provided by patchOperations that have already been processed"

	^ aPatchOperation requirements difference: self provided
! !

Object subclass: #CypressLoader
	instanceVariableNames: 'additions removals unloadable provisions errors methodAdditions requirements'
	package: 'Cypress-Definitions'!

!CypressLoader methodsFor: 'accessing'!

additions

	additions ifNil: [ additions := OrderedCollection new ].
	^additions
!

errors
	errors ifNil: [ errors := OrderedCollection new ].
	^errors
!

methodAdditions

	^#()
!

provisions
	^ provisions ifNil: [provisions := (Smalltalk current classes collect: [:cl | cl name]) asSet ]
!

removals

	removals ifNil: [ removals := OrderedCollection new ].
	^removals
!

unloadable

	unloadable ifNil: [ unloadable := OrderedCollection new ].
	^unloadable
! !

!CypressLoader methodsFor: 'applying'!

applyAddition: aCypressPatchOperation

	self additions add: aCypressPatchOperation
!

applyModification: aCypressPatchOperation

	self additions add: aCypressPatchOperation
!

applyRemoval: aCypressPatchOperation

	self removals add: aCypressPatchOperation
! !

!CypressLoader methodsFor: 'error handling'!

handleErrorFor: aPatchOperation during: aBlock
	aBlock on: Error do: [:ex | self errors add: aPatchOperation ].
! !

!CypressLoader methodsFor: 'loading'!

analyze

	self 
		analyzeAdditions;
		analyzeRemovals
!

analyzeAdditions

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self additions;
		addExternalProvisions: self provisions;
		yourself.
	additions := sorter orderedItems.
	requirements := sorter externalRequirements.
	unloadable := sorter itemsWithMissingRequirements.
!

analyzeRemovals

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self removals;
		yourself.
	removals := sorter orderedItems reversed.
!

basicLoad
	errorDefinitions := OrderedCollection new.
	self additions do: [:ea | self loadClassDefinition: ea ]. "load class definitions first"
	self additions do: [:ea | self loadMethodDefinition: ea ] . "load method definitions now"
	self removals do: [:ea | self unloadDefinition: ea ]. "now we can remove things"
	self errors do: [:ea | ea addMethodAdditionTo: methodAdditions]. "not sure about methodAddtions...yet"
	self methodAdditions do: [:ea | self loadMethodAddition: ea ]. "ditto"
	self additions do: [:ea | self postLoad: ea ]. "this is where the obsoletion is taken into account ..."
!

load

	self analyze.
	self unloadable isEmpty ifFalse: [self unloadableDefinitionsError].
	self basicLoad
!

updatePackage: aPackage withSnapshot: aSnapshot
	|  patch snapshot |
	snapshot := aPackage snapshot.
	patch := aSnapshot patchRelativeToBase: snapshot.
	patch applyTo: self.
	snapshot definitions do: [:ea | self provisions addAll: ea provisions]
! !

!CypressLoader methodsFor: 'operations'!

loadClassDefinition: aPatchOperation

	self 
		handleErrorFor: aPatchOperation 
		during: [ aPatchOperation loadClassDefinition ]
!

loadMethodDefinition: aPatchOperation
	
	self 
		handleErrorFor: aPatchOperation 
		during: [ aPatchOperation loadMethodDefinition ]
!

postLoad: aPatchOperation
	aPatchOperation postLoadDefinition
!

unloadDefinition: aPatchOperation
	
	self 
		handleErrorFor: aPatchOperation 
		during: [ aPatchOperation unloadDefinition ]
! !

!CypressLoader class methodsFor: 'loading'!

updatePackage: aPackage withSnapshot: aSnapshot
	self new
		updatePackage: aPackage withSnapshot: aSnapshot;
		load
! !

Object subclass: #CypressPackageDefinition
	instanceVariableNames: 'name'
	package: 'Cypress-Definitions'!

!CypressPackageDefinition methodsFor: 'accessing'!

name
	^ name
!

name: aString
	name := aString
! !

!CypressPackageDefinition methodsFor: 'comparing'!

= other
	^ other species = self species and: [other name sameAs: name]
! !

!CypressPackageDefinition methodsFor: 'printing'!

printString
	^super printString, '(', name, ')'
! !

!CypressPackageDefinition methodsFor: 'snapshotting'!

snapshot
	| package definitions name map  |
	package := Package named: self name.
	definitions := OrderedCollection new.
	package sortedClasses do: [:cls |
        	definitions add: cls asCypressClassDefinition.
                ((cls methodDictionary values) sorted: [:a :b | a selector <= b selector]) do: [:method |
			(method category match: '^\*') ifFalse: [ 
				definitions add: method asCypressMethodDefinition ]].
                ((cls class methodDictionary values) sorted: [:a :b | a selector <= b selector]) do: [:method |
			(method category match: '^\*') ifFalse: [ 
				definitions add: method asCypressMethodDefinition ]]].
	name := package name.
	(Package sortedClasses: Smalltalk current classes) do: [:each |
		{each. each class} do: [:aClass |
			map := Dictionary new.
			aClass protocolsDo: [:category :methods | 
				(category match: '^\*', name) ifTrue: [ map at: category put: methods ]].
			(map keys sorted: [:a :b | a <= b ])  do: [:category | 
				((map at: category) sorted: [:a :b | a selector <= b selector]) do: [:method |  definitions add: method asCypressMethodDefinition ]]]].
	^ CypressSnapshot definitions: definitions
! !

Object subclass: #CypressPatch
	instanceVariableNames: 'operations'
	package: 'Cypress-Definitions'!

!CypressPatch methodsFor: 'accessing'!

operations

	^operations
! !

!CypressPatch methodsFor: 'applying'!

applyTo: aCypressLoader
	operations do: [:ea | ea applyTo: aCypressLoader].
! !

!CypressPatch methodsFor: 'initialization'!

fromBase: baseSnapshot toTarget: targetSnapshot
	| base target |	
	operations := OrderedCollection new.
	base := CypressDefinitionIndex definitions: baseSnapshot definitions.
	target := CypressDefinitionIndex definitions: targetSnapshot definitions.
	
	target definitions do:
		[:t |
		base
			definitionLike: t
			ifPresent: [:b | (b isSameRevisionAs: t) ifFalse: [operations add: (CypressModification of: b to: t)]]
			ifAbsent: [operations add: (CypressAddition of: t)]].
		
	base definitions do:
		[:b |
		target
			definitionLike: b
			ifPresent: [:t | ]
			ifAbsent: [operations add: (CypressRemoval of: b)]]
! !

!CypressPatch class methodsFor: 'instance creation'!

fromBase: baseSnapshot toTarget: targetSnapshot
	^ (self new)
		fromBase: baseSnapshot
		toTarget: targetSnapshot
! !

Object subclass: #CypressPatchOperation
	instanceVariableNames: ''
	package: 'Cypress-Definitions'!

!CypressPatchOperation methodsFor: 'accessing'!

description

	self subclassResponsibility
! !

!CypressPatchOperation methodsFor: 'applying'!

applyTo: aCypressLoader

	self subclassResponsibility
! !

!CypressPatchOperation methodsFor: 'comparing'!

= aPatchOperation
	^aPatchOperation isKindOf: self class
! !

!CypressPatchOperation methodsFor: 'dependency'!

provisions
	"Answer list of global names defined by this definition"

	self subclassResponsibility
!

requirements
	"Answer list of global names required by this definition"

	self subclassResponsibility
! !

!CypressPatchOperation methodsFor: 'loading'!

loadClassDefinition

	self subclassResponsibility
!

loadMethodDefinition
	self subclassResponsibility
!

postLoadDefinition
	self subclassResponsibility
!

unloadDefinition

	self error: 'inappropriate to send #unloadDefinition to an addition or modification operation'
! !

CypressPatchOperation subclass: #CypressAddition
	instanceVariableNames: 'definition'
	package: 'Cypress-Definitions'!

!CypressAddition methodsFor: 'accessing'!

definition

	^definition
!

description

	^'add: ', self definition description
! !

!CypressAddition methodsFor: 'applying'!

applyTo: aCypressLoader

	aCypressLoader applyAddition: self
! !

!CypressAddition methodsFor: 'comparing'!

= aPatchOperation
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]
! !

!CypressAddition methodsFor: 'dependency'!

provisions
	"Answer list of global names defined by this definition"

	^self definition provisions
!

requirements
	"Answer list of global names required by this definition"

	^self definition requirements
! !

!CypressAddition methodsFor: 'initialization'!

definition: aDefinition

	definition := aDefinition
! !

!CypressAddition methodsFor: 'loading'!

loadClassDefinition

	self definition loadClassDefinition
!

loadMethodDefinition
	self definition loadMethodDefinition
!

postLoadDefinition
	self definition postLoadOver: nil
! !

!CypressAddition methodsFor: 'printing'!

printString

	| str |
	str := StringStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self description;
		nextPutAll: ')'.
	^str contents
! !

!CypressAddition class methodsFor: 'instance creation'!

of: aDefinition
	^ self new definition: aDefinition
! !

CypressPatchOperation subclass: #CypressModification
	instanceVariableNames: 'obsoletion modification'
	package: 'Cypress-Definitions'!

!CypressModification methodsFor: 'accessing'!

description

	^'modify from: ', self obsoletion description, ' to: ', self modification description
!

modification

	^modification
!

obsoletion

	^obsoletion
! !

!CypressModification methodsFor: 'applying'!

applyTo: aCypressLoader

	aCypressLoader applyModification: self
! !

!CypressModification methodsFor: 'dependency'!

provisions
	"Answer list of global names defined by this definition"

	^self modification provisions
!

requirements
	"Answer list of global names required by this definition"

	^self modification requirements
! !

!CypressModification methodsFor: 'initialization'!

= aPatchOperation
	^(super = aPatchOperation) and: [self obsoletion = aPatchOperation obsoletion and: [ self modification = aPatchOperation modification]]
!

base: base target: target

	obsoletion := base.
	modification := target.
! !

!CypressModification methodsFor: 'loading'!

loadClassDefinition

	self modification loadClassDefinition
!

loadMethodDefinition
	self modification loadMethodDefinition
!

postLoadDefinition
	self modification postLoadOver: self obsoletion
! !

!CypressModification methodsFor: 'printing'!

printString

	| str |
	str := StringStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self description;
		nextPutAll: ')'.
	^str contents
! !

!CypressModification class methodsFor: 'instance creation'!

of: base to: target
	^ self new base: base target: target
! !

CypressPatchOperation subclass: #CypressRemoval
	instanceVariableNames: 'definition'
	package: 'Cypress-Definitions'!

!CypressRemoval methodsFor: 'accessing'!

definition

	^definition
!

description

	^'remove: ', self definition description
! !

!CypressRemoval methodsFor: 'applying'!

applyTo: aCypressLoader

	aCypressLoader applyRemoval: self
! !

!CypressRemoval methodsFor: 'comparing'!

= aPatchOperation
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]
! !

!CypressRemoval methodsFor: 'dependency'!

provisions
	"Answer list of global names defined by this definition"

	^#()
!

requirements
	"Answer list of global names required by this definition"

	^#()
! !

!CypressRemoval methodsFor: 'initialization'!

definition: aDefinition

	definition := aDefinition
! !

!CypressRemoval methodsFor: 'loading'!

loadClassDefinition
	
	self error: 'inappropriate to send #loadClassDefinition to a removal operation'
!

loadMethodDefinition
	
	self error: 'inappropriate to send #loadMethodDefinition to a removal operation'
!

postLoadDefinition
	
	self error: 'inappropriate to send #postLoadDefinition to a removal operation'
!

unloadDefinition

	self definition unloadDefinition
! !

!CypressRemoval methodsFor: 'printing'!

printString

	| str |
	str := StringStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self description;
		nextPutAll: ')'.
	^str contents
! !

!CypressRemoval class methodsFor: 'instance creation'!

of: aDefinition
	^ self new definition: aDefinition
! !

Object subclass: #CypressSnapshot
	instanceVariableNames: 'definitions'
	package: 'Cypress-Definitions'!

!CypressSnapshot methodsFor: 'accessing'!

definitions

	^definitions
!

definitions: aDefinitions

	definitions := aDefinitions
! !

!CypressSnapshot methodsFor: 'comparing'!

= other
	^ definitions asArray = other definitions asArray
! !

!CypressSnapshot methodsFor: 'enumerating'!

classDefinitions: classBlock methodDefinitions: methodBlock

	self definitions do: [:definition |
		definition classDefinition: classBlock methodDefinition: methodBlock]
! !

!CypressSnapshot methodsFor: 'loading'!

updatePackage: aPackage
	CypressLoader updatePackage: aPackage withSnapshot: self
! !

!CypressSnapshot methodsFor: 'patching'!

patchRelativeToBase: aSnapshot
	^ CypressPatch fromBase: aSnapshot toTarget: self
! !

!CypressSnapshot class methodsFor: 'instance creation'!

definitions: aDefinitions

	^(self new) definitions: aDefinitions
! !

!Object methodsFor: '*Cypress-Definitions'!

species

	^self class
! !

!Class methodsFor: '*Cypress-Definitions'!

asCypressClassDefinition
	^CypressClassDefinition
		name: self name
		superclassName: self superclass name
		category: self category 
		instVarNames: self instanceVariableNames
		classInstVarNames: self class instanceVariableNames
		comment: self comment
! !

!Collection methodsFor: '*Cypress-Definitions'!

difference: aCollection
	"Answer the set theoretic difference of two collections."

	^ self reject: [:each | aCollection includes: each]
!

gather: aBlock
	"Answer a collection containing all the results of evaluating aBlock, collected together."

	| array |
	array := Array new.
	self do: [:ea | array addAll: (aBlock value: ea)]
!

intersection: aCollection
	"Answer the set theoretic intersection of two collections."

	^ self select: [:each | aCollection includes: each]
! !

!CharacterArray methodsFor: '*Cypress-Definitions'!

sameAs: aString

	^self asUppercase = aString asUppercase
! !

!CompiledMethod methodsFor: '*Cypress-Definitions'!

asCypressMethodDefinition

	^CypressMethodDefinition 
        	className: (self methodClass isMetaclass ifTrue: [ self methodClass instanceClass ] ifFalse: [ self methodClass ]) name
		classIsMeta: self methodClass isMetaclass
		selector: self selector
		category: self category
		source: self source
! !

