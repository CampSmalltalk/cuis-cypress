'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 23 April 2012 at 8:40:45 pm'!
'Description Please enter a description for this package '!
!classDefinition: #CypressJsonParser category: #'Cypress-Structure'!
Object subclass: #CypressJsonParser
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!
!classDefinition: 'CypressJsonParser class' category: #'Cypress-Structure'!
CypressJsonParser class
	instanceVariableNames: ''!

!classDefinition: #CypressPackageReader category: #'Cypress-Structure'!
Object subclass: #CypressPackageReader
	instanceVariableNames: 'packageDirectory packageStructure properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!
!classDefinition: 'CypressPackageReader class' category: #'Cypress-Structure'!
CypressPackageReader class
	instanceVariableNames: ''!

!classDefinition: #CypressPackageWriter category: #'Cypress-Structure'!
Object subclass: #CypressPackageWriter
	instanceVariableNames: 'packageStructure rootDirectory packageDirectory'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!
!classDefinition: 'CypressPackageWriter class' category: #'Cypress-Structure'!
CypressPackageWriter class
	instanceVariableNames: 'specials'!

!classDefinition: #CypressStructure category: #'Cypress-Structure'!
Object subclass: #CypressStructure
	instanceVariableNames: 'name properties packageStructure'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!
!classDefinition: 'CypressStructure class' category: #'Cypress-Structure'!
CypressStructure class
	instanceVariableNames: ''!

!classDefinition: #CypressClassStructure category: #'Cypress-Structure'!
CypressStructure subclass: #CypressClassStructure
	instanceVariableNames: 'instanceMethods classMethods comment isClassExtension'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!
!classDefinition: 'CypressClassStructure class' category: #'Cypress-Structure'!
CypressClassStructure class
	instanceVariableNames: ''!

!classDefinition: #CypressMethodStructure category: #'Cypress-Structure'!
CypressStructure subclass: #CypressMethodStructure
	instanceVariableNames: 'source isMetaclass classStructure'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!
!classDefinition: 'CypressMethodStructure class' category: #'Cypress-Structure'!
CypressMethodStructure class
	instanceVariableNames: ''!

!classDefinition: #CypressPackageStructure category: #'Cypress-Structure'!
CypressStructure subclass: #CypressPackageStructure
	instanceVariableNames: 'classes extensions'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!
!classDefinition: 'CypressPackageStructure class' category: #'Cypress-Structure'!
CypressPackageStructure class
	instanceVariableNames: ''!


!Array methodsFor: '*Cypress-Structure'!
asCypressPropertyObject

	^self collect: [:each | each asCypressPropertyObject ]
! !

!Array methodsFor: '*Cypress-Structure'!
writeCypressJsonOn: aStream  indent: startIndent

	| indent |
	aStream 
		nextPutAll: '[';
		lf.
	indent := startIndent + 1.
	1 to: self size do: [:index | | item | 
		item := self at: index.
		aStream tab: indent.
		item writeCypressJsonOn: aStream  indent: indent.
		index < self size ifTrue: [ aStream nextPutAll: ','; lf ]].
	self size = 0 ifTrue: [ aStream tab: indent ].
	aStream nextPutAll: ' ]'
! !

!Boolean methodsFor: '*Cypress-Structure'!
writeCypressJsonOn: aStream  indent: startIndent

	aStream 
		nextPutAll: self printString
! !

!CypressClassStructure methodsFor: 'converting'!
asCypressClassDefinition
	self isClassExtension ifTrue: [ ^nil ].
	^CypressClassDefinition
		name: self className
		superclassName: self superclassName
		category: self category 
		instVarNames: self instanceVariableNames
		classInstVarNames: self classInstanceVariableNames
		comment: self comment
! !

!CypressClassStructure methodsFor: 'accessing'!
category

	^self packageStructure packageName
! !

!CypressClassStructure methodsFor: 'accessing'!
classInstanceVariableNames
	^self properties at: 'classinstvars' ifAbsent: ['']
! !

!CypressClassStructure methodsFor: 'accessing'!
classInstanceVariableNames: aString
	^self properties at: 'classinstvars' put: aString
! !

!CypressClassStructure methodsFor: 'querying'!
classMethodNamed: methodName

	^self classMethods 
		at: methodName 
		ifAbsent: [ self classMethods at: methodName put: (CypressMethodStructure new name: methodName) ]
! !

!CypressClassStructure methodsFor: 'accessing'!
classMethods

	classMethods ifNil: [ classMethods := Dictionary new ].
	^classMethods
! !

!CypressClassStructure methodsFor: 'accessing'!
className

	^self name
! !

!CypressClassStructure methodsFor: 'accessing'!
comment

	comment ifNil: [ comment := '' ].
	^comment
! !

!CypressClassStructure methodsFor: 'accessing'!
comment: aString

	comment := aString
! !

!CypressClassStructure methodsFor: 'initialization'!
fromClassDefinition: classDefinition

	self isClassExtension: false.
	self name: classDefinition name.
	self comment: classDefinition comment.
  	self superclassName: classDefinition superclassName.
	self instanceVariableNames: classDefinition instVarNames.
	self classInstanceVariableNames: classDefinition classInstVarNames.
! !

!CypressClassStructure methodsFor: 'initialization'!
fromJs: jsObject

	properties := jsObject at: 'properties.json'.
	(jsObject at: 'class' ifAbsent: [#()]) do: [:jsMethodObject |  | methodNameParts |
		methodNameParts := self splitMethodNameFor: jsMethodObject.
		(self classMethodNamed: (methodNameParts at: 1)) 
			packageStructure: self packageStructure;
			classStructure: self;
			isMetaclass: true;
			fromJs: jsMethodObject named: methodNameParts ].
	(jsObject at: 'instance' ifAbsent: [#()]) do: [:jsMethodObject |  | methodNameParts |
		methodNameParts := self splitMethodNameFor: jsMethodObject.
		(self instanceMethodNamed: (methodNameParts at: 1)) 
			packageStructure: self packageStructure;
			classStructure: self;
			fromJs: jsMethodObject named: methodNameParts ].	
	comment := jsObject at: 'README.md' ifAbsent: ['']
! !

!CypressClassStructure methodsFor: 'querying'!
instanceMethodNamed: methodName

	^self instanceMethods 
		at: methodName 
		ifAbsent: [ self instanceMethods at: methodName put: (CypressMethodStructure new name: methodName) ]
! !

!CypressClassStructure methodsFor: 'accessing'!
instanceMethods

	instanceMethods ifNil: [ instanceMethods := Dictionary new ].
	^instanceMethods
! !

!CypressClassStructure methodsFor: 'accessing'!
instanceVariableNames

	^self properties at: 'instvars' ifAbsent: ['']
! !

!CypressClassStructure methodsFor: 'accessing'!
instanceVariableNames: aString

	^self properties at: 'instvars' put: aString
! !

!CypressClassStructure methodsFor: 'accessing'!
isClassExtension

        isClassExtension ifNil: [ isClassExtension := true ].
        ^isClassExtension
! !

!CypressClassStructure methodsFor: 'accessing'!
isClassExtension: aBoolean

	isClassExtension := aBoolean
! !

!CypressClassStructure methodsFor: 'accessing'!
name

	^self properties at: 'name'
! !

!CypressClassStructure methodsFor: 'accessing'!
name: aString

	self properties at: 'name' put: aString
! !

!CypressClassStructure methodsFor: 'private'!
splitMethodNameFor: jsMethodObject

	| ext methodName |
	methodName := jsMethodObject at: 'name'.
	ext := '.json'.
	(methodName match: ext, '$')
		ifFalse: [
			ext := '.st'.
			(methodName match: ext, '$')
				ifFalse: [ self error: 'invalid structure element: ', methodName ] ].
	^{methodName copyFrom: 1 to: (methodName size - ext size). ext}
! !

!CypressClassStructure methodsFor: 'accessing'!
superclassName

	^self properties at: 'super'
! !

!CypressClassStructure methodsFor: 'accessing'!
superclassName: aString

	^self properties at: 'super' put: aString
! !

!CypressClassStructure methodsFor: 'writing'!
writeJsonOn: aStream  indent: startIndent

	| indent methods |
	indent := startIndent.
	aStream 
		tab: indent;
		nextPutAll: '{';
		lf.
	indent := indent + 1.
	aStream
		tab: indent;
		nextPutAll: '"name"';
		nextPutAll: ' : ';
		nextPutAll: '"', self name, (self isClassExtension ifTrue: [ '.extension' ] ifFalse: [ '.class' ]), '",';
		lf.
	aStream
		tab: indent;
		nextPutAll: '"instance" : [';
		lf;
		yourself.
	methods := self instanceMethods values asArray sorted: [:a :b | a selector <= b selector].
	1 to: methods size do: [:index | | methodStructure | 
		methodStructure := methods at: index.
		methodStructure writeJsonOn: aStream indent: indent + 1.
		index < methods size ifTrue: [ aStream nextPutAll: ','; lf ]].
	aStream
		tab: indent;
		nextPutAll: '],';
		lf;
		yourself.
	aStream
		tab: indent;
		nextPutAll: '"class" : [';
		lf;
		yourself.
	methods := self classMethods values asArray sorted: [:a :b | a selector <= b selector].
	1 to: methods size do: [:index | | methodStructure | 
		methodStructure := methods at: index.
		methodStructure writeJsonOn: aStream indent: indent + 1.
		index < methods size ifTrue: [ aStream nextPutAll: ','; lf ]].
	aStream
		tab: indent;
		nextPutAll: ']'.
	self isClassExtension
		ifFalse: [ 
			aStream
				nextPutAll: ',';
				lf;
				tab: indent;
				nextPutAll: '"README.md" : ';
				yourself.
			self comment writeCypressJsonOn: aStream indent: indent ].
	aStream
		nextPutAll: ',';
		lf;
		tab: indent;
		nextPutAll: '"properties.json" : ';
		yourself.
	self properties writeCypressJsonOn: aStream indent: indent.
	indent := indent - 1.
	aStream
		lf;
		tab: indent;
		nextPutAll: ' }'
! !

!CypressClassStructure class methodsFor: 'instance creation'!
fromClassDefinition: classDefinition

	^self new
		fromClassDefinition: classDefinition;
		yourself
! !

!CypressJsonParser methodsFor: 'adding' stamp: 'dkh 2/16/2012 14:39:25'!
addProperty: anAssociation to: anObject

!CypressJsonParser methodsFor: 'adding' stamp: 'dkh 2/16/2012 14:39:25'!
addValue: anObject to: aCollection

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createArray

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createFalse

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createNull

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createNumber: aString

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createObject

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createProperty: aKey with: aValue

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createString: aString

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createTrue

!CypressJsonParser methodsFor: 'private' stamp: 'dkh 2/16/2012 14:39:25'!
expect: aString

!CypressJsonParser methodsFor: 'initialization' stamp: 'dkh 2/16/2012 14:39:25'!
initializeOn: aStream

!CypressJsonParser methodsFor: 'private' stamp: 'dkh 2/16/2012 14:39:25'!
match: aString

!CypressJsonParser methodsFor: 'parsing' stamp: 'dkh 2/16/2012 14:39:25'!
parse

!CypressJsonParser methodsFor: 'parsing' stamp: 'dkh 2/16/2012 14:39:25'!
parseArray

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 2/16/2012 14:39:25'!
parseCharacter

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 2/16/2012 14:39:25'!
parseCharacterHex

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 4/6/2012 15:56:14'!
parseCharacterHexDigit

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 2/16/2012 14:39:25'!
parseNumber

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 4/6/2012 15:56:14'!
parseNumberExponent

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 4/6/2012 15:56:14'!
parseNumberFraction

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 4/6/2012 15:56:14'!
parseNumberInteger

!CypressJsonParser methodsFor: 'parsing' stamp: 'dkh 2/16/2012 14:39:25'!
parseObject

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 2/16/2012 14:39:25'!
parseProperty

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 2/16/2012 14:39:25'!
parseString

!CypressJsonParser methodsFor: 'parsing' stamp: 'dkh 2/16/2012 14:39:25'!
parseValue

!CypressJsonParser methodsFor: 'private' stamp: 'dkh 2/16/2012 14:39:25'!
whitespace

!CypressJsonParser class methodsFor: 'instance creation' stamp: 'dkh 2/16/2012 14:39:25'!
new

!CypressJsonParser class methodsFor: 'instance creation' stamp: 'dkh 2/16/2012 14:39:25'!
on: aStream

!CypressJsonParser class methodsFor: 'accessing' stamp: 'dkh 2/16/2012 14:39:25'!
parse: aString

!CypressJsonParser class methodsFor: 'accessing' stamp: 'dkh 2/16/2012 14:39:25'!
parseStream: aStream

!CypressMethodStructure methodsFor: 'converting'!
asCypressMethodDefinition

	^CypressMethodDefinition 
        	className: self classStructure className
		classIsMeta: self isMetaclass
		selector: self selector
		category: self category
		source: self source
! !

!CypressMethodStructure methodsFor: 'accessing'!
category

	^self properties at: 'category'
! !

!CypressMethodStructure methodsFor: 'accessing'!
category: aString

	self properties at: 'category' put: aString
! !

!CypressMethodStructure methodsFor: 'accessing'!
classStructure
	^classStructure
! !

!CypressMethodStructure methodsFor: 'accessing'!
classStructure: aCypressClassStructure
	classStructure := aCypressClassStructure
! !

!CypressMethodStructure methodsFor: 'accessing' stamp: 'dkh 4/23/2012 20:24'!
cypressSource

	| stream |
	stream := WriteStream on: String new.
	stream 
		nextPutAll: self category;
		lf;
		nextPutAll: self source.
	^stream contents
! !

!CypressMethodStructure methodsFor: 'private' stamp: 'dkh 4/23/2012 20:25'!
extractCypressSource: aString

	| stream categoryStream sourceStream readingCategory |
	stream := ReadStream on: aString.
	stream reset.
	categoryStream := WriteStream on: String new.
	sourceStream := WriteStream on: String new.
	readingCategory := true.
	[ stream atEnd] whileFalse: [ | char |
		char := stream next.
		readingCategory
			ifTrue: [
				char = String lf
					ifTrue: [ readingCategory := false ]
					ifFalse: [ categoryStream nextPutAll: char ]]
			ifFalse: [ sourceStream nextPutAll: char ]].
	self category: categoryStream contents.
	self source: sourceStream contents
! !

!CypressMethodStructure methodsFor: 'initialization'!
fromJs: jsObject  named: methodNameParts

	| ext |
	(ext := methodNameParts at: 2) = '.st'
		ifTrue: [  self extractCypressSource: (jsObject at: 'contents') ]
		ifFalse: [ ext = '.json' ifTrue: [  properties := jsObject at: 'contents' ] ]
! !

!CypressMethodStructure methodsFor: 'initialization'!
fromMethodDefinition: methodDefinition

	self isMetaclass: methodDefinition classIsMeta.
	self selector: methodDefinition selector.
	self category: methodDefinition category.
	self source: methodDefinition source.
! !

!CypressMethodStructure methodsFor: 'accessing'!
isMetaclass

	isMetaclass ifNil: [ isMetaclass := false ].
	^isMetaclass
! !

!CypressMethodStructure methodsFor: 'accessing'!
isMetaclass: aBoolean
	isMetaclass := aBoolean
! !

!CypressMethodStructure methodsFor: 'accessing'!
selector

	^String
		streamContents: [:stream | self name do: [:chara | stream nextPutAll: (chara = '.' ifTrue:  [ ':' ] ifFalse: [ chara ])]]
! !

!CypressMethodStructure methodsFor: 'accessing'!
selector: aString

	name := String
		streamContents: [:stream | aString do: [:chara | stream nextPutAll: (chara = ':' ifTrue:  [ '.' ] ifFalse: [ chara ])]]
! !

!CypressMethodStructure methodsFor: 'accessing'!
source

	^source
! !

!CypressMethodStructure methodsFor: 'accessing'!
source: aString

	source := aString
! !

!CypressMethodStructure methodsFor: 'writing'!
writeJsonOn: aStream  indent: startIndent

	| indent |
	indent := startIndent.
	aStream 
		tab: indent;
		nextPutAll: '{';
		lf.
	indent := indent + 1.
	aStream
		tab: indent;
		nextPutAll: '"name"';
		nextPutAll: ' : ';
		nextPutAll: '"', self name, '.st",';
		lf.
	aStream
		tab: indent;
		nextPutAll: '"contents"';
		nextPutAll: ' : '.
	self cypressSource writeCypressJsonOn: aStream indent: indent.
	indent := indent - 1.
	aStream
		lf;
		tab: indent;
		nextPutAll: ' }'
! !

!CypressMethodStructure class methodsFor: 'instance creation'!
fromMethodDefinition: methodDefinition

	^self new
		fromMethodDefinition: methodDefinition;
		yourself
! !

!CypressPackageReader methodsFor: 'private' stamp: 'dkh 4/22/2012 13:24:15'!
classStructureFrom: classPropertiesDict 

!CypressPackageReader methodsFor: 'private' stamp: 'dkh 4/22/2012 13:24:15'!
classStructureFrom: classPropertiesDict comment: classComment.

!CypressPackageReader methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageDirectory

!CypressPackageReader methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageDirectory: aDirectory

!CypressPackageReader methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageStructure

!CypressPackageReader methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageStructure: aPackageStructure

!CypressPackageReader methodsFor: 'reading' stamp: 'dkh 4/22/2012 13:24:15'!
read

!CypressPackageReader methodsFor: 'reading' stamp: 'dkh 4/23/2012 20:19'!
readClassStructureFromEntry: classEntry

!CypressPackageReader methodsFor: 'reading' stamp: 'dkh 4/23/2012 20:20'!
readExtensionClassStructureFromEntry: classEntry

!CypressPackageReader methodsFor: 'reading' stamp: 'dkh 4/22/2012 13:24:15'!
readMethodStructureFor: classStructure in: entries

!CypressPackageReader methodsFor: 'reading' stamp: 'dkh 4/22/2012 13:24:15'!
readPackageStructure

!CypressPackageReader methodsFor: 'reading' stamp: 'dkh 4/23/2012 20:20'!
readPropertiesFile	

!CypressPackageReader class methodsFor: 'instance creation' stamp: 'dkh 4/22/2012 13:24:15'!
readPackageStructureFrom: aPackagesDirectory

!CypressPackageStructure methodsFor: 'accessing'!
classes

	classes ifNil: [ classes := OrderedCollection new ].
	^classes
! !

!CypressPackageStructure methodsFor: 'accessing'!
extensions

	extensions ifNil: [ extensions := OrderedCollection new ].
	^extensions
! !

!CypressPackageStructure methodsFor: 'initialization'!
fromJs: jsObject

	name := jsObject at: 'name'.
	(jsObject at: 'contents') do: [:jsClassObject| | classStructure objectName |
		classStructure := (CypressClassStructure new)
                		packageStructure: self;
				yourself.
                ((objectName := jsClassObject at: 'name') match: '.extension$')
			ifTrue: [ 
				classStructure isClassExtension: true.
				self extensions add: classStructure ]
			ifFalse: [
				(objectName match: '.class$')
					ifTrue: [ 
						classStructure isClassExtension: false.
						self classes add: classStructure ]].
		classStructure fromJs: jsClassObject].
	properties := jsObject at: 'properties.json'
! !

!CypressPackageStructure methodsFor: 'initialization'!
fromPackage: aCypressPackageDefinition

	| snapshot classMap classDefinitions classStructure |
	snapshot := aCypressPackageDefinition snapshot.
	name := aCypressPackageDefinition name, '.package'.
	properties := Dictionary new.
	classDefinitions := Set new.
	classMap := Dictionary new.
	snapshot definitions do: [:definition |  
			definition 
				classDefinition: [:classDefinition |  classDefinitions add: classDefinition ] 
				methodDefinition: [:methodDefinition | 
					(classMap 
						at: methodDefinition className 
						ifAbsent: [classMap at: methodDefinition className put: Set new]) 
							add: methodDefinition. ]].
	classDefinitions do: [:classDefinition |
		classStructure := (CypressClassStructure fromClassDefinition: classDefinition)
			packageStructure: self.
		(classMap removeKey: classDefinition className ifAbsent: [#()]) do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self classes add: classStructure ].
	classMap keysAndValuesDo: [:className :methods |
		classStructure := (CypressClassStructure new name: className)
			packageStructure: self.
		methods do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self extensions add: classStructure ].
! !

!CypressPackageStructure methodsFor: 'accessing'!
packageExtension

	^self properties at: 'extension' ifAbsent: ['.package' ]
! !

!CypressPackageStructure methodsFor: 'accessing'!
packageName

	^self name copyFrom: 1 to: (self name size - self packageExtension size)
! !

!CypressPackageStructure methodsFor: 'accessing'!
packageStructure
	^self
! !

!CypressPackageStructure methodsFor: 'snapshotting'!
snapshot
	| definitions map  |
	definitions := OrderedCollection new.
	self classes do: [:classStructure |
        	definitions add: classStructure asCypressClassDefinition.
                (classStructure instanceMethods values asArray sorted: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods values asArray sorted: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ]].
	self extensions do: [:classStructure |
                (classStructure instanceMethods values asArray sorted: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods values asArray sorted: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ]].
	^ CypressSnapshot definitions: definitions
! !

!CypressPackageStructure methodsFor: 'writing'!
writeJsonOn: aStream  indent: startIndent

	| indent |
	indent := startIndent.
	aStream 
		tab: indent;
		nextPutAll: '{';
		lf.
	indent := indent + 1.
	aStream
		tab: indent;
		nextPutAll: '"name"';
		nextPutAll: ' : ';
		nextPutAll: '"', self name, '",'.
	aStream
		lf;
		tab: indent;
		nextPutAll: '"contents" : [';
		lf;
		yourself.
	1 to: self classes size do: [:index | | classStructure | 
		classStructure := self classes at: index.
		classStructure writeJsonOn: aStream indent: indent + 1.
		(self extensions size > 0 or: [ index < self classes size]) ifTrue: [ aStream nextPutAll: ','; lf. ]].
	1 to: self extensions size do: [:index | | classStructure | 
		classStructure := self extensions at: index.
		classStructure writeJsonOn: aStream indent: indent + 1.
		index < self extensions size ifTrue: [ aStream nextPutAll: ','; lf.] ].
	aStream
		lf;
		tab: indent;
		nextPutAll: '],';
		lf;
		tab: indent;
		nextPutAll: '"properties.json" : '.
	self properties writeCypressJsonOn: aStream indent: indent.
	indent := indent - 1.
	aStream 
		lf;
		tab: indent;
		nextPutAll: '}'
! !

!CypressPackageStructure class methodsFor: 'instance creation'!
fromJson: aJsonString

	^self fromJs: (Compiler new eval: '(', aJsonString , ')')
! !

!CypressPackageStructure class methodsFor: 'instance creation'!
fromPackage: aCypressPackageDefinition

	^(self new) 
		fromPackage: aCypressPackageDefinition;
		yourself
! !

!CypressPackageWriter methodsFor: 'private' stamp: 'dkh 4/22/2012 13:24:15'!
directoryForDirectoryNamed: directoryNameOrPath

!CypressPackageWriter methodsFor: 'private' stamp: 'dkh 4/22/2012 13:24:15'!
fileNameForSelector: selector

!CypressPackageWriter methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageDirectory

!CypressPackageWriter methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageDirectory: aPackageDirectory

!CypressPackageWriter methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageStructure

!CypressPackageWriter methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageStructure: aCypressPackageStructure

!CypressPackageWriter methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
rootDirectory

!CypressPackageWriter methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
rootDirectory: aDirectory

!CypressPackageWriter methodsFor: 'writing' stamp: 'dkh 4/22/2012 13:24:15'!
write

!CypressPackageWriter methodsFor: 'writing' stamp: 'dkh 4/22/2012 13:24:15'!
writeClassComment: classStructure on: fileStream

!CypressPackageWriter methodsFor: 'writing' stamp: 'dkh 4/22/2012 13:24:15'!
writeClassStructure: classStructure on: fileStream

!CypressPackageWriter methodsFor: 'writing' stamp: 'dkh 4/22/2012 13:24:15'!
writeClassStructure: classStructure to: classPath

!CypressPackageWriter methodsFor: 'writing' stamp: 'dkh 4/22/2012 13:24:15'!
writeExtensionClassStructure: classStructure to: classPath

!CypressPackageWriter methodsFor: 'private' stamp: 'dkh 4/22/2012 13:24:15'!
writeInDirectoryName: directoryNameOrPath fileName: fileName extension: ext visit: visitBlock

!CypressPackageWriter methodsFor: 'writing' stamp: 'dkh 4/22/2012 13:24:15'!
writeMethodStructure: methodStructure to:methodPath

!CypressPackageWriter methodsFor: 'writing' stamp: 'dkh 4/22/2012 13:24:15'!
writePackageStructure

!CypressPackageWriter methodsFor: 'writing' stamp: 'dkh 4/22/2012 13:24:15'!
writePackageStructureClasses:  classStructures isClassExtension: isClassExtension

!CypressPackageWriter methodsFor: 'writing' stamp: 'dkh 4/22/2012 13:24:15'!
writePropertiesFile

!CypressPackageWriter class methodsFor: 'as yet unclassified' stamp: 'dkh 4/22/2012 13:24:15'!
initializeSpecials

!CypressPackageWriter class methodsFor: 'as yet unclassified' stamp: 'dkh 4/22/2012 13:24:15'!
specials

!CypressPackageWriter class methodsFor: 'instance creation' stamp: 'dkh 4/22/2012 13:24:15'!
writePackageStructure: aPackageStructure to: aPackagesDirectory

!CypressStructure methodsFor: 'initialization'!
fromJs: jsObject

	self subclassResponsibility
! !

!CypressStructure methodsFor: 'initialization' stamp: 'dkh 4/23/2012 19:15'!
fromPackage: aCypressPackageDefinition

	| snapshot classMap classDefinitions classStructure |
	snapshot := aCypressPackageDefinition snapshot.
	name := aCypressPackageDefinition name, '.package'.
	properties := Dictionary new.
	classDefinitions := Set new.
	classMap := Dictionary new.
	snapshot definitions do: [:definition |  
			definition 
				classDefinition: [:classDefinition |  classDefinitions add: classDefinition ] 
				methodDefinition: [:methodDefinition | 
					(classMap 
						at: methodDefinition className 
						ifAbsent: [classMap at: methodDefinition className put: Set new]) 
							add: methodDefinition. ]].
	classDefinitions do: [:classDefinition | 
		classStructure := (CypressClassStructure fromClassDefinition: classDefinition)
			packageStructure: self.
		(classMap removeKey: classDefinition className ifAbsent: [#()]) do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self classes add: classStructure ].
	classMap keysAndValuesDo: [:className :methods |
		classStructure := (CypressClassStructure new name: className)
			packageStructure: self.
		methods do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self extensions add: classStructure ].
! !

!CypressStructure methodsFor: 'accessing'!
name

	^name
! !

!CypressStructure methodsFor: 'accessing'!
name: aString 

	name := aString
! !

!CypressStructure methodsFor: 'accessing'!
packageStructure
	^packageStructure
! !

!CypressStructure methodsFor: 'accessing'!
packageStructure: aCypressPackageStructure
	packageStructure := aCypressPackageStructure
! !

!CypressStructure methodsFor: 'writing'!
path: aFSPath file: aFilename write: writeBlock

	| fs stream |
	fs := aFSPath fs.
	stream := fs createWriteStream: (aFSPath resolve: aFilename).
	writeBlock value: stream.
	stream end.
! !

!CypressStructure methodsFor: 'accessing'!
properties

	properties ifNil: [ properties := Dictionary new ].
	^properties
! !

!CypressStructure methodsFor: 'writing'!
writeJsonOn: aStream

	self writeJsonOn: aStream indent: 0.
! !

!CypressStructure methodsFor: 'writing'!
writeJsonOn: aStream  indent: indent

	self subclassResponsibility
! !

!CypressStructure class methodsFor: 'instance creation'!
fromJs: jsObject

	^(self new) 
		fromJs: jsObject asCypressPropertyObject;
		yourself
! !

!Dictionary methodsFor: '*Cypress-Structure'!
writeCypressJsonOn: aStream  indent: startIndent
	| indent count |
	indent := startIndent.
	aStream 
		nextPutAll: '{';
		lf.
	count := 0.
	indent := indent + 1.
	self keysAndValuesDo: [:key :value |
		count := count+ 1.
		aStream tab: indent.
		key writeCypressJsonOn: aStream indent: indent.
		aStream nextPutAll: ' : '.
		value writeCypressJsonOn: aStream indent: indent.
		count < self size ifTrue: [ aStream nextPutAll: ','; lf ] ].
	self size = 0 ifTrue: [ aStream tab: indent ].
	aStream nextPutAll: ' }'
! !

!Number methodsFor: '*Cypress-Structure'!
writeCypressJsonOn: aStream  indent: startIndent

	aStream 
		nextPutAll: self printString
! !

!Object methodsFor: '*Cypress-Structure'!
asCypressPropertyObject

	^self
! !

!String methodsFor: '*Cypress-Structure'!
asCypressPropertyObject

	^self unescaped
! !

!String methodsFor: '*Cypress-Structure'!
writeCypressJsonOn: aStream  indent: startIndent

	aStream 
		nextPutAll: '"';
		nextPutAll: self escaped;
		nextPutAll: '"'
! !