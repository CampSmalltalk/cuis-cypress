!classDefinition: #CypressDefinition category: #'Cypress-Definitions'!
Object subclass: #CypressDefinition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

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
! !

!CypressDefinition methodsFor: 'dependency'!
requirements
	"Answer list of global names required by this definition"

	^#()
! !


!CypressDefinition methodsFor: 'loading'!
actualClass

	self subclassResponsibility
! !

!CypressDefinition methodsFor: 'loading'!
loadClassDefinition
	"default is to do nothing"
! !

!CypressDefinition methodsFor: 'loading'!
loadMethodDefinition
	"default is to do nothing"
! !

!CypressDefinition methodsFor: 'loading'!
postLoad
	"noop"
! !

!CypressDefinition methodsFor: 'loading'!
postLoadOver: aDefinition

	self postLoad
! !

!CypressDefinition methodsFor: 'loading'!
unloadDefinition

	self subclassResponsibility
! !


!CypressDefinition methodsFor: 'testing'!
isRevisionOf: aDefinition
	^ (aDefinition isKindOf: CypressDefinition) and: [aDefinition description = self description]
! !

!CypressDefinition methodsFor: 'testing'!
isSameRevisionAs: aDefinition
	^ self = aDefinition
! !


!CypressDefinition methodsFor: 'visiting'!
classDefinition: classBlock methodDefinition: methodBlock
	"default is noop"
! !


!classDefinition: #CypressClassDefinition category: #'Cypress-Definitions'!
CypressDefinition subclass: #CypressClassDefinition
	instanceVariableNames: 'name superclassName category comment instVarNames classInstVarNames'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressClassDefinition methodsFor: 'accessing'!
category

	^category
! !

!CypressClassDefinition methodsFor: 'accessing'!
classInstVarNames

	^classInstVarNames
! !

!CypressClassDefinition methodsFor: 'accessing'!
className

	^self name
! !

!CypressClassDefinition methodsFor: 'accessing'!
comment

	^comment
! !

!CypressClassDefinition methodsFor: 'accessing'!
description

	^ Array with: name
! !

!CypressClassDefinition methodsFor: 'accessing'!
instVarNames

	^instVarNames
! !

!CypressClassDefinition methodsFor: 'accessing'!
name

	^name
! !

!CypressClassDefinition methodsFor: 'accessing'!
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
! !

!CypressClassDefinition methodsFor: 'dependency'!
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
! !

!CypressClassDefinition methodsFor: 'loading'!
createClass

	| superClass |
	superClass := Smalltalk current at: self superclassName.
	^ClassBuilder new
		superclass: superClass 
		subclass: self name
		instanceVariableNames: (self stringForVariables: self instVarNames)
		classVariableNames: '' poolDictionaries: '' category: self category
! !

!CypressClassDefinition methodsFor: 'loading'!
loadClassDefinition

	 | cls |
	cls := self createClass.
	cls class instanceVariableNames: (self stringForVariables: self classInstVarNames).
	self comment notEmpty ifTrue: [ cls comment: self comment ]
! !

!CypressClassDefinition methodsFor: 'loading'!
unloadDefinition

	Smalltalk current removeClass: self actualClass.
! !


!CypressClassDefinition methodsFor: 'printString' stamp: 'dkh 4/23/2012 20:24'!
printString

	| str |
	str := WriteStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self name;
		nextPutAll: ')'.
	^str contents
! !

!CypressClassDefinition methodsFor: 'printString'!
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

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CypressClassDefinition class' category: #'Cypress-Definitions'!
CypressClassDefinition class
	instanceVariableNames: ''!

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


!classDefinition: #CypressDefinitionIndex category: #'Cypress-Definitions'!
Object subclass: #CypressDefinitionIndex
	instanceVariableNames: 'definitionMap'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressDefinitionIndex methodsFor: 'accessing'!
definitionMap
	definitionMap ifNil: [ definitionMap := Dictionary new ].
	^ definitionMap
! !

!CypressDefinitionIndex methodsFor: 'accessing'!
definitions
	^self definitionMap values
! !


!CypressDefinitionIndex methodsFor: 'adding'!
add: aDefinition
	^ self definitionMap at: aDefinition description put: aDefinition
! !

!CypressDefinitionIndex methodsFor: 'adding'!
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

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CypressDefinitionIndex class' category: #'Cypress-Definitions'!
CypressDefinitionIndex class
	instanceVariableNames: ''!

!CypressDefinitionIndex class methodsFor: 'instance creation'!
definitions: aCollection
	^ self new addAll: aCollection
! !


!classDefinition: #CypressDependencySorter category: #'Cypress-Definitions'!
Object subclass: #CypressDependencySorter
	instanceVariableNames: 'required provided orderedItems'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressDependencySorter methodsFor: 'accessing'!
externalRequirements
	| unloaded providedByUnloaded |
	unloaded := self itemsWithMissingRequirements.
	providedByUnloaded := (unloaded gather: [:e | e provisions]) asSet.
	^ self required keys reject: [:globalName | providedByUnloaded includes: globalName ]
! !

!CypressDependencySorter methodsFor: 'accessing'!
itemsWithMissingRequirements
	| patchOperations |
	patchOperations := Set new.
	self required values do: [:aSetOfPatchOperations | patchOperations addAll: aSetOfPatchOperations ].
	^ patchOperations
! !

!CypressDependencySorter methodsFor: 'accessing'!
orderedItems
	"ordered list of patch operations"

	orderedItems ifNil: [ orderedItems := OrderedCollection new ].
	^orderedItems
! !

!CypressDependencySorter methodsFor: 'accessing'!
provided
	"set of global names provided by definitions already loaded"

	provided ifNil: [ provided := Set new ].
	^provided
! !

!CypressDependencySorter methodsFor: 'accessing'!
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
! !

!CypressDependencySorter methodsFor: 'building'!
addAll: aCollection
	aCollection do: [:aPatchOperation | self add: aPatchOperation ]
! !


!CypressDependencySorter methodsFor: 'private'!
addExternalProvisions: aCollection
	(aCollection intersection: self externalRequirements)
		do: [:globalName | self addProvision: globalName]
! !

!CypressDependencySorter methodsFor: 'private'!
addProvision: aGlobalName
	| newlySatisfied |
	self provided add: aGlobalName.
	newlySatisfied := self required removeKey: aGlobalName ifAbsent: [#()].
	self addAll: newlySatisfied.
! !

!CypressDependencySorter methodsFor: 'private'!
addRequirement: globalName for: aPatchOperation
	(self itemsRequiring: globalName) add: aPatchOperation
! !

!CypressDependencySorter methodsFor: 'private'!
addRequirements: aCollection for: aPatchOperation
	aCollection do: [:globalName | self addRequirement: globalName for: aPatchOperation]
! !

!CypressDependencySorter methodsFor: 'private'!
addToOrder: aPatchOperation
	self orderedItems add: aPatchOperation.
	aPatchOperation provisions do: [:globalName | self addProvision: globalName ].
! !

!CypressDependencySorter methodsFor: 'private'!
itemsRequiring: globalName
	^ self required at: globalName ifAbsentPut: [Set new]
! !

!CypressDependencySorter methodsFor: 'private'!
unresolvedRequirementsFor: aPatchOperation
	"Answer a list of global names that are required by <aPatchOperation>, but not 
	 provided by patchOperations that have already been processed"

	^ aPatchOperation requirements difference: self provided
! !


!classDefinition: #CypressLoader category: #'Cypress-Definitions'!
Object subclass: #CypressLoader
	instanceVariableNames: 'additions removals unloadable provisions errors methodAdditions requirements'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressLoader methodsFor: 'accessing'!
additions

	additions ifNil: [ additions := OrderedCollection new ].
	^additions
! !

!CypressLoader methodsFor: 'accessing'!
errors
	errors ifNil: [ errors := OrderedCollection new ].
	^errors
! !

!CypressLoader methodsFor: 'accessing'!
methodAdditions

	^#()
! !

!CypressLoader methodsFor: 'accessing'!
provisions
	^ provisions ifNil: [provisions := (Smalltalk current classes collect: [:cl | cl name]) asSet ]
! !

!CypressLoader methodsFor: 'accessing'!
removals

	removals ifNil: [ removals := OrderedCollection new ].
	^removals
! !

!CypressLoader methodsFor: 'accessing'!
unloadable

	unloadable ifNil: [ unloadable := OrderedCollection new ].
	^unloadable
! !


!CypressLoader methodsFor: 'applying'!
applyAddition: aCypressPatchOperation

	self additions add: aCypressPatchOperation
! !

!CypressLoader methodsFor: 'applying'!
applyModification: aCypressPatchOperation

	self additions add: aCypressPatchOperation
! !

!CypressLoader methodsFor: 'applying'!
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
! !

!CypressLoader methodsFor: 'loading'!
analyzeAdditions

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self additions;
		addExternalProvisions: self provisions;
		yourself.
	additions := sorter orderedItems.
	requirements := sorter externalRequirements.
	unloadable := sorter itemsWithMissingRequirements.
! !

!CypressLoader methodsFor: 'loading'!
analyzeRemovals

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self removals;
		yourself.
	removals := sorter orderedItems reversed.
! !

!CypressLoader methodsFor: 'loading' stamp: 'dkh 4/23/2012 20:26'!
basicLoad
	errors := OrderedCollection new.
	self additions do: [:ea | self loadClassDefinition: ea ]. "load class definitions first"
	self additions do: [:ea | self loadMethodDefinition: ea ] . "load method definitions now"
	self removals do: [:ea | self unloadDefinition: ea ]. "now we can remove things"
	self errors do: [:ea | ea addMethodAdditionTo: methodAdditions]. "not sure about methodAddtions...yet"
	self methodAdditions do: [:ea | self loadMethodAddition: ea ]. "ditto"
	self additions do: [:ea | self postLoad: ea ]. "this is where the obsoletion is taken into account ..."
! !

!CypressLoader methodsFor: 'loading'!
load

	self analyze.
	self unloadable isEmpty ifFalse: [self unloadableDefinitionsError].
	self basicLoad
! !

!CypressLoader methodsFor: 'loading'!
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
! !

!CypressLoader methodsFor: 'operations'!
loadMethodDefinition: aPatchOperation
	
	self 
		handleErrorFor: aPatchOperation 
		during: [ aPatchOperation loadMethodDefinition ]
! !

!CypressLoader methodsFor: 'operations'!
postLoad: aPatchOperation
	aPatchOperation postLoadDefinition
! !

!CypressLoader methodsFor: 'operations'!
unloadDefinition: aPatchOperation
	
	self 
		handleErrorFor: aPatchOperation 
		during: [ aPatchOperation unloadDefinition ]
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CypressLoader class' category: #'Cypress-Definitions'!
CypressLoader class
	instanceVariableNames: ''!

!CypressLoader class methodsFor: 'loading'!
updatePackage: aPackage withSnapshot: aSnapshot
	self new
		updatePackage: aPackage withSnapshot: aSnapshot;
		load
! !


!classDefinition: #CypressMethodDefinition category: #'Cypress-Definitions'!
CypressDefinition subclass: #CypressMethodDefinition
	instanceVariableNames: 'classIsMeta source category selector className'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressMethodDefinition methodsFor: 'accessing'!
category

	^category
! !

!CypressMethodDefinition methodsFor: 'accessing'!
classIsMeta

	^classIsMeta
! !

!CypressMethodDefinition methodsFor: 'accessing'!
className

	^className
! !

!CypressMethodDefinition methodsFor: 'accessing'!
description
	^ Array	
		with: className
		with: selector
		with: classIsMeta
! !

!CypressMethodDefinition methodsFor: 'accessing'!
selector

	^selector
! !

!CypressMethodDefinition methodsFor: 'accessing'!
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
! !

!CypressMethodDefinition methodsFor: 'loading'!
loadMethodDefinition

	self actualClass
		compile: self source
		category: self category
! !

!CypressMethodDefinition methodsFor: 'loading'!
postLoadOver: aDefinition

	super postLoadOver: aDefinition.
	(self isInitializer
		and: [ aDefinition isNil or: [ self source ~= aDefinition source ]]) 
			ifTrue: [ self theNonMetaClass initialize ].
! !

!CypressMethodDefinition methodsFor: 'loading'!
theNonMetaClass
	^Smalltalk current at: self className
! !

!CypressMethodDefinition methodsFor: 'loading'!
unloadDefinition

	| cls |
	cls := self actualClass.
	cls removeCompiledMethod: (cls methodAt: self selector)
! !


!CypressMethodDefinition methodsFor: 'printing' stamp: 'dkh 4/23/2012 20:24'!
printString

	| str |
	str := WriteStream on: String new.
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
! !

!CypressMethodDefinition methodsFor: 'visiting'!
instanceMethod: instanceBlock classMethod: classBlock

	^(self classIsMeta
		ifTrue: [ classBlock ]
		ifFalse: [ instanceBlock ]) value: self
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CypressMethodDefinition class' category: #'Cypress-Definitions'!
CypressMethodDefinition class
	instanceVariableNames: ''!

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


!classDefinition: #CypressPackageDefinition category: #'Cypress-Definitions'!
Object subclass: #CypressPackageDefinition
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressPackageDefinition methodsFor: 'accessing'!
name
	^ name
! !

!CypressPackageDefinition methodsFor: 'accessing'!
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


!classDefinition: #CypressPatch category: #'Cypress-Definitions'!
Object subclass: #CypressPatch
	instanceVariableNames: 'operations'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

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

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CypressPatch class' category: #'Cypress-Definitions'!
CypressPatch class
	instanceVariableNames: ''!

!CypressPatch class methodsFor: 'instance creation'!
fromBase: baseSnapshot toTarget: targetSnapshot
	^ (self new)
		fromBase: baseSnapshot
		toTarget: targetSnapshot
! !


!classDefinition: #CypressPatchOperation category: #'Cypress-Definitions'!
Object subclass: #CypressPatchOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

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
! !

!CypressPatchOperation methodsFor: 'dependency'!
requirements
	"Answer list of global names required by this definition"

	self subclassResponsibility
! !


!CypressPatchOperation methodsFor: 'loading'!
loadClassDefinition

	self subclassResponsibility
! !

!CypressPatchOperation methodsFor: 'loading'!
loadMethodDefinition
	self subclassResponsibility
! !

!CypressPatchOperation methodsFor: 'loading'!
postLoadDefinition
	self subclassResponsibility
! !

!CypressPatchOperation methodsFor: 'loading'!
unloadDefinition

	self error: 'inappropriate to send #unloadDefinition to an addition or modification operation'
! !


!classDefinition: #CypressAddition category: #'Cypress-Definitions'!
CypressPatchOperation subclass: #CypressAddition
	instanceVariableNames: 'definition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressAddition methodsFor: 'accessing'!
definition

	^definition
! !

!CypressAddition methodsFor: 'accessing'!
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
! !

!CypressAddition methodsFor: 'dependency'!
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
! !

!CypressAddition methodsFor: 'loading'!
loadMethodDefinition
	self definition loadMethodDefinition
! !

!CypressAddition methodsFor: 'loading'!
postLoadDefinition
	self definition postLoadOver: nil
! !


!CypressAddition methodsFor: 'printing' stamp: 'dkh 4/23/2012 20:24'!
printString

	| str |
	str := WriteStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self description;
		nextPutAll: ')'.
	^str contents
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CypressAddition class' category: #'Cypress-Definitions'!
CypressAddition class
	instanceVariableNames: ''!

!CypressAddition class methodsFor: 'instance creation'!
of: aDefinition
	^ self new definition: aDefinition
! !


!classDefinition: #CypressModification category: #'Cypress-Definitions'!
CypressPatchOperation subclass: #CypressModification
	instanceVariableNames: 'obsoletion modification'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressModification methodsFor: 'accessing'!
description

	^'modify from: ', self obsoletion description, ' to: ', self modification description
! !

!CypressModification methodsFor: 'accessing'!
modification

	^modification
! !

!CypressModification methodsFor: 'accessing'!
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
! !

!CypressModification methodsFor: 'dependency'!
requirements
	"Answer list of global names required by this definition"

	^self modification requirements
! !


!CypressModification methodsFor: 'initialization'!
= aPatchOperation
	^(super = aPatchOperation) and: [self obsoletion = aPatchOperation obsoletion and: [ self modification = aPatchOperation modification]]
! !

!CypressModification methodsFor: 'initialization'!
base: base target: target

	obsoletion := base.
	modification := target.
! !


!CypressModification methodsFor: 'loading'!
loadClassDefinition

	self modification loadClassDefinition
! !

!CypressModification methodsFor: 'loading'!
loadMethodDefinition
	self modification loadMethodDefinition
! !

!CypressModification methodsFor: 'loading'!
postLoadDefinition
	self modification postLoadOver: self obsoletion
! !


!CypressModification methodsFor: 'printing' stamp: 'dkh 4/23/2012 20:25'!
printString

	| str |
	str := WriteStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self description;
		nextPutAll: ')'.
	^str contents
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CypressModification class' category: #'Cypress-Definitions'!
CypressModification class
	instanceVariableNames: ''!

!CypressModification class methodsFor: 'instance creation'!
of: base to: target
	^ self new base: base target: target
! !


!classDefinition: #CypressRemoval category: #'Cypress-Definitions'!
CypressPatchOperation subclass: #CypressRemoval
	instanceVariableNames: 'definition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressRemoval methodsFor: 'accessing'!
definition

	^definition
! !

!CypressRemoval methodsFor: 'accessing'!
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
! !

!CypressRemoval methodsFor: 'dependency'!
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
! !

!CypressRemoval methodsFor: 'loading'!
loadMethodDefinition
	
	self error: 'inappropriate to send #loadMethodDefinition to a removal operation'
! !

!CypressRemoval methodsFor: 'loading'!
postLoadDefinition
	
	self error: 'inappropriate to send #postLoadDefinition to a removal operation'
! !

!CypressRemoval methodsFor: 'loading'!
unloadDefinition

	self definition unloadDefinition
! !


!CypressRemoval methodsFor: 'printing' stamp: 'dkh 4/23/2012 20:25'!
printString

	| str |
	str := WriteStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self description;
		nextPutAll: ')'.
	^str contents
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CypressRemoval class' category: #'Cypress-Definitions'!
CypressRemoval class
	instanceVariableNames: ''!

!CypressRemoval class methodsFor: 'instance creation'!
of: aDefinition
	^ self new definition: aDefinition
! !


!classDefinition: #CypressSnapshot category: #'Cypress-Definitions'!
Object subclass: #CypressSnapshot
	instanceVariableNames: 'definitions'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressSnapshot methodsFor: 'accessing'!
definitions

	^definitions
! !

!CypressSnapshot methodsFor: 'accessing'!
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

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CypressSnapshot class' category: #'Cypress-Definitions'!
CypressSnapshot class
	instanceVariableNames: ''!

!CypressSnapshot class methodsFor: 'instance creation'!
definitions: aDefinitions

	^(self new) definitions: aDefinitions
! !
