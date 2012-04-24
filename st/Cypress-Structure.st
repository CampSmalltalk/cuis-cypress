Smalltalk current createPackage: 'Cypress-Structure' properties: #{}!
Object subclass: #CypressStructure
	instanceVariableNames: 'name properties packageStructure'
	package: 'Cypress-Structure'!

!CypressStructure methodsFor: 'accessing'!

name

	^name
!

name: aString 

	name := aString
!

packageStructure
	^packageStructure
!

packageStructure: aCypressPackageStructure
	packageStructure := aCypressPackageStructure
!

properties

	properties ifNil: [ properties := Dictionary new ].
	^properties
! !

!CypressStructure methodsFor: 'initialization'!

fromJs: jsObject

	self subclassResponsibility
! !

!CypressStructure methodsFor: 'writing'!

path: aFSPath file: aFilename write: writeBlock

	| fs stream |
	fs := aFSPath fs.
	stream := fs createWriteStream: (aFSPath resolve: aFilename).
	writeBlock value: stream.
	stream end.
!

writeJsonOn: aStream

	self writeJsonOn: aStream indent: 0.
!

writeJsonOn: aStream  indent: indent

	self subclassResponsibility
! !

!CypressStructure class methodsFor: 'instance creation'!

fromJs: jsObject

	^(self new) 
		fromJs: jsObject asCypressPropertyObject;
		yourself
! !

CypressStructure subclass: #CypressClassStructure
	instanceVariableNames: 'instanceMethods classMethods comment isClassExtension'
	package: 'Cypress-Structure'!

!CypressClassStructure methodsFor: 'accessing'!

category

	^self packageStructure packageName
!

classInstanceVariableNames
	^self properties at: 'classinstvars' ifAbsent: ['']
!

classInstanceVariableNames: aString
	^self properties at: 'classinstvars' put: aString
!

classMethods

	classMethods ifNil: [ classMethods := Dictionary new ].
	^classMethods
!

className

	^self name
!

comment

	comment ifNil: [ comment := '' ].
	^comment
!

comment: aString

	comment := aString
!

instanceMethods

	instanceMethods ifNil: [ instanceMethods := Dictionary new ].
	^instanceMethods
!

instanceVariableNames

	^self properties at: 'instvars' ifAbsent: ['']
!

instanceVariableNames: aString

	^self properties at: 'instvars' put: aString
!

isClassExtension

        isClassExtension ifNil: [ isClassExtension := true ].
        ^isClassExtension
!

isClassExtension: aBoolean

	isClassExtension := aBoolean
!

name

	^self properties at: 'name'
!

name: aString

	self properties at: 'name' put: aString
!

superclassName

	^self properties at: 'super'
!

superclassName: aString

	^self properties at: 'super' put: aString
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

!CypressClassStructure methodsFor: 'initialization'!

fromClassDefinition: classDefinition

	self isClassExtension: false.
	self name: classDefinition name.
	self comment: classDefinition comment.
  	self superclassName: classDefinition superclassName.
	self instanceVariableNames: classDefinition instVarNames.
	self classInstanceVariableNames: classDefinition classInstVarNames.
!

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

!CypressClassStructure methodsFor: 'querying'!

classMethodNamed: methodName

	^self classMethods 
		at: methodName 
		ifAbsent: [ self classMethods at: methodName put: (CypressMethodStructure new name: methodName) ]
!

instanceMethodNamed: methodName

	^self instanceMethods 
		at: methodName 
		ifAbsent: [ self instanceMethods at: methodName put: (CypressMethodStructure new name: methodName) ]
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

CypressStructure subclass: #CypressMethodStructure
	instanceVariableNames: 'source isMetaclass classStructure'
	package: 'Cypress-Structure'!

!CypressMethodStructure methodsFor: 'accessing'!

category

	^self properties at: 'category'
!

category: aString

	self properties at: 'category' put: aString
!

classStructure
	^classStructure
!

classStructure: aCypressClassStructure
	classStructure := aCypressClassStructure
!

cypressSource

	| stream |
	stream := StringStream on: String new.
	stream 
		nextPutAll: self category;
		lf;
		nextPutAll: self source.
	^stream contents
!

isMetaclass

	isMetaclass ifNil: [ isMetaclass := false ].
	^isMetaclass
!

isMetaclass: aBoolean
	isMetaclass := aBoolean
!

selector

	^String
		streamContents: [:stream | self name do: [:chara | stream nextPutAll: (chara = '.' ifTrue:  [ ':' ] ifFalse: [ chara ])]]
!

selector: aString

	name := String
		streamContents: [:stream | aString do: [:chara | stream nextPutAll: (chara = ':' ifTrue:  [ '.' ] ifFalse: [ chara ])]]
!

source

	^source
!

source: aString

	source := aString
! !

!CypressMethodStructure methodsFor: 'converting'!

asCypressMethodDefinition

	^CypressMethodDefinition 
        	className: self classStructure className
		classIsMeta: self isMetaclass
		selector: self selector
		category: self category
		source: self source
! !

!CypressMethodStructure methodsFor: 'initialization'!

fromJs: jsObject  named: methodNameParts

	| ext |
	(ext := methodNameParts at: 2) = '.st'
		ifTrue: [  self extractCypressSource: (jsObject at: 'contents') ]
		ifFalse: [ ext = '.json' ifTrue: [  properties := jsObject at: 'contents' ] ]
!

fromMethodDefinition: methodDefinition

	self isMetaclass: methodDefinition classIsMeta.
	self selector: methodDefinition selector.
	self category: methodDefinition category.
	self source: methodDefinition source.
! !

!CypressMethodStructure methodsFor: 'private'!

extractCypressSource: aString

	| stream categoryStream sourceStream readingCategory |
	stream := StringStream on: aString.
	stream reset.
	categoryStream := StringStream on: String new.
	sourceStream := StringStream on: String new.
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

CypressStructure subclass: #CypressPackageStructure
	instanceVariableNames: 'classes extensions'
	package: 'Cypress-Structure'!

!CypressPackageStructure methodsFor: 'accessing'!

classes

	classes ifNil: [ classes := OrderedCollection new ].
	^classes
!

extensions

	extensions ifNil: [ extensions := OrderedCollection new ].
	^extensions
!

packageExtension

	^self properties at: 'extension' ifAbsent: ['.package' ]
!

packageName

	^self name copyFrom: 1 to: (self name size - self packageExtension size)
!

packageStructure
	^self
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
!

fromPackage: aCypressPackageDefinition

	| snapshot classMap classDefinitions |
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
	classDefinitions do: [:classDefinition | | classStructure |
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
!

fromPackage: aCypressPackageDefinition

	^(self new) 
		fromPackage: aCypressPackageDefinition;
		yourself
! !

!Object methodsFor: '*Cypress-Structure'!

asCypressPropertyObject

	^self
! !

!Boolean methodsFor: '*Cypress-Structure'!

writeCypressJsonOn: aStream  indent: startIndent

	aStream 
		nextPutAll: self printString
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

!Array methodsFor: '*Cypress-Structure'!

asCypressPropertyObject

	^self collect: [:each | each asCypressPropertyObject ]
!

writeCypressJsonOn: aStream  indent: indent

	aStream 
		nextPutAll: '[';
		lf.
	indent := indent + 1.
	1 to: self size do: [:index | | item | 
		item := self at: index.
		aStream tab: indent.
		item writeCypressJsonOn: aStream  indent: indent.
		index < self size ifTrue: [ aStream nextPutAll: ','; lf ]].
	self size = 0 ifTrue: [ aStream tab: indent ].
	aStream nextPutAll: ' ]'
! !

!Array methodsFor: '*Cypress-Structure'!

asCypressPropertyObject

	^self collect: [:each | each asCypressPropertyObject ]
!

writeCypressJsonOn: aStream  indent: indent

	aStream 
		nextPutAll: '[';
		lf.
	indent := indent + 1.
	1 to: self size do: [:index | | item | 
		item := self at: index.
		aStream tab: indent.
		item writeCypressJsonOn: aStream  indent: indent.
		index < self size ifTrue: [ aStream nextPutAll: ','; lf ]].
	self size = 0 ifTrue: [ aStream tab: indent ].
	aStream nextPutAll: ' ]'
! !

!String methodsFor: '*Cypress-Structure'!

asCypressPropertyObject

	^self unescaped
!

writeCypressJsonOn: aStream  indent: startIndent

	aStream 
		nextPutAll: '"';
		nextPutAll: self escaped;
		nextPutAll: '"'
! !

!JSObjectProxy methodsFor: '*Cypress-Structure'!

asCypressPropertyObject
	| properties |
	properties := Dictionary new.
	<for(var i in self['@jsObject']) {
		properties._at_put_(i, self['@jsObject'][i]);
	}>.
	properties associations do: [:assoc |
		properties 
			at: assoc key 
			put: assoc value asCypressPropertyObject ].
	^properties
! !

!Number methodsFor: '*Cypress-Structure'!

writeCypressJsonOn: aStream  indent: startIndent

	aStream 
		nextPutAll: self printString
! !

!StringStream methodsFor: '*Cypress-Structure'!

tab
	self nextPut: '	'
!

tab: count
	count timesRepeat: [ self tab ]
! !

