'From Cuis 4.0 of 21 April 2012 [latest update: #1306] on 13 June 2012 at 9:15:12 am'!
'Description Install after Cypress-Definitions. Includes package reader and writer.

View class comments for CypressPackageReader and CypressPackageWriter'!
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


!CypressPackageReader commentStamp: '<historical>' prior: 0!
Reader for the Cypress multi-dialect file format for Smalltalk packages

	CypressPackageReader installAsCodePackage: (FileDirectory default directoryNamed: 'Cypress-Mocks.package')!

!CypressPackageWriter commentStamp: 'jmv 6/13/2012 09:13' prior: 0!
Writer for the Cypress multi-dialect file format for Smalltalk packages

	CypressPackageWriter writeCodePackage: (CodePackage named: 'Cypress-Mocks' createIfAbsent: true registerIfNew: false)
	CypressPackageWriter writeCodePackage: (CodePackage named: 'Cypress-Structure' createIfAbsent: true registerIfNew: false)
	CypressPackageWriter writeCodePackage: (CodePackage named: 'Morphic' createIfAbsent: true registerIfNew: false)!

!Array methodsFor: '*Cypress-Structure'!
asCypressPropertyObject

	^self collect: [:each | each asCypressPropertyObject ]
! !

!Array methodsFor: '*Cypress-Structure' stamp: 'dkh 4/23/2012 23:36'!
writeCypressJsonOn: aStream  indent: startIndent

	| indent |
	aStream 
		nextPutAll: '[';
		newLine.
	indent := startIndent + 1.
	1 to: self size do: [:index | | item | 
		item := self at: index.
		aStream tab: indent.
		item writeCypressJsonOn: aStream  indent: indent.
		index < self size ifTrue: [ aStream nextPutAll: ','; newLine ]].
	self size = 0 ifTrue: [ aStream tab: indent ].
	aStream nextPutAll: ' ]'
! !

!Boolean methodsFor: '*Cypress-Structure'!
writeCypressJsonOn: aStream  indent: startIndent

	aStream 
		nextPutAll: self printString
! !

!Character methodsFor: '*Cypress-Structure' stamp: 'dkh 4/23/2012 23:50'!
isSafeForHTTP
	"whether a character is 'safe', or needs to be escaped when used, eg, in a URL"

	^  value < 128
		and: [ self isAlphaNumeric
				or: [ '.-_' includes: self ]]
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

!CypressClassStructure methodsFor: 'accessing' stamp: 'jmv 6/6/2012 23:07'!
properties: classPropertiesDict
	properties _ classPropertiesDict! !

!CypressClassStructure methodsFor: 'private' stamp: 'dkh 4/23/2012 23:25'!
splitMethodName: methodName

	| ext  |
	ext := '.json'.
	(   '*' , ext match: methodName)
		ifFalse: [
			ext := '.st'.
			('*' , ext match: methodName)
				ifFalse: [ self error: 'invalid structure element: ', methodName ] ].
	^{methodName copyFrom: 1 to: (methodName size - ext size). ext}
! !

!CypressClassStructure methodsFor: 'private' stamp: 'dkh 4/23/2012 23:24'!
splitMethodNameFor: jsMethodObject

	^self splitMethodName: (jsMethodObject at: 'name')! !

!CypressClassStructure methodsFor: 'accessing'!
superclassName

	^self properties at: 'super'
! !

!CypressClassStructure methodsFor: 'accessing'!
superclassName: aString

	^self properties at: 'super' put: aString
! !

!CypressClassStructure methodsFor: 'writing' stamp: 'dkh 4/23/2012 23:38'!
writeJsonOn: aStream  indent: startIndent

	| indent methods |
	indent := startIndent.
	aStream 
		tab: indent;
		nextPutAll: '{';
		newLine.
	indent := indent + 1.
	aStream
		tab: indent;
		nextPutAll: '"name"';
		nextPutAll: ' : ';
		nextPutAll: '"', self name, (self isClassExtension ifTrue: [ '.extension' ] ifFalse: [ '.class' ]), '",';
		newLine.
	aStream
		tab: indent;
		nextPutAll: '"instance" : [';
		newLine;
		yourself.
	methods := self instanceMethods values asArray sorted: [:a :b | a selector <= b selector].
	1 to: methods size do: [:index | | methodStructure | 
		methodStructure := methods at: index.
		methodStructure writeJsonOn: aStream indent: indent + 1.
		index < methods size ifTrue: [ aStream nextPutAll: ','; newLine ]].
	aStream
		tab: indent;
		nextPutAll: '],';
		newLine;
		yourself.
	aStream
		tab: indent;
		nextPutAll: '"class" : [';
		newLine;
		yourself.
	methods := self classMethods values asArray sorted: [:a :b | a selector <= b selector].
	1 to: methods size do: [:index | | methodStructure | 
		methodStructure := methods at: index.
		methodStructure writeJsonOn: aStream indent: indent + 1.
		index < methods size ifTrue: [ aStream nextPutAll: ','; newLine ]].
	aStream
		tab: indent;
		nextPutAll: ']'.
	self isClassExtension
		ifFalse: [ 
			aStream
				nextPutAll: ',';
				newLine;
				tab: indent;
				nextPutAll: '"README.md" : ';
				yourself.
			self comment writeCypressJsonOn: aStream indent: indent ].
	aStream
		nextPutAll: ',';
		newLine;
		tab: indent;
		nextPutAll: '"properties.json" : ';
		yourself.
	self properties writeCypressJsonOn: aStream indent: indent.
	indent := indent - 1.
	aStream
		newLine;
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
	"Add the property anAssociation described with key and value to anObject. Subclasses might want to refine this implementation."
	
	^ anObject 
		add: anAssociation;
		yourself! !

!CypressJsonParser methodsFor: 'adding' stamp: 'dkh 2/16/2012 14:39:25'!
addValue: anObject to: aCollection
	"Add anObject to aCollection. Subclasses might want to refine this implementation."

	^ aCollection copyWith: anObject! !

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createArray
	"Create an empty collection. Subclasses might want to refine this implementation."

	^ Array new! !

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createFalse
	"Create the false literal. Subclasses might want to refine this implementation."
	
	^ false! !

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createNull
	"Create the null literal. Subclasses might want to refine this implementation."

	^ nil! !

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createNumber: aString
	"Create a number literal. Subclasses might want to refine this implementation."

	^ aString asNumber! !

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createObject
	"Create an empty object. Subclasses might want to refine this implementation."
	
	^ Dictionary new! !

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createProperty: aKey with: aValue
	"Create an empty attribute value pair. Subclasses might want to refine this implementation."
	
	^ aKey -> aValue! !

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createString: aString
	"Create a string literal. Subclasses might want to refine this implementation."

	^ aString! !

!CypressJsonParser methodsFor: 'creating' stamp: 'dkh 2/16/2012 14:39:25'!
createTrue
	"Create the true literal. Subclasses might want to refine this implementation."

	^ true! !

!CypressJsonParser methodsFor: 'private' stamp: 'dkh 2/16/2012 14:39:25'!
expect: aString
	"Expects aString and consume input, throw an error otherwise."

	^ (self match: aString) ifFalse: [ self error: aString , ' expected' ]! !

!CypressJsonParser methodsFor: 'initialization' stamp: 'dkh 2/16/2012 14:39:25'!
initializeOn: aStream
	self initialize.
	stream := aStream! !

!CypressJsonParser methodsFor: 'private' stamp: 'dkh 2/16/2012 14:39:25'!
match: aString
	"Tries to match aString, consume input and answer true if successful."
	
	| position |
	position := stream position.
	aString do: [ :each |
		(stream atEnd or: [ stream next ~= each ]) ifTrue: [ 
			stream position: position.
			^ false ] ].
	self whitespace.
	^ true! !

!CypressJsonParser methodsFor: 'parsing' stamp: 'dkh 2/16/2012 14:39:25'!
parse
	| result |
	result := self whitespace; parseValue.
	stream atEnd
		ifFalse: [ self error: 'end of input expected' ].
	^ result! !

!CypressJsonParser methodsFor: 'parsing' stamp: 'dkh 2/16/2012 14:39:25'!
parseArray
	| result |
	self expect: '['.
	result := self createArray.
	(self match: ']')
		ifTrue: [ ^ result ].
	[ stream atEnd ] whileFalse: [
		result := self
			addValue: self parseValue
			to: result.
		(self match: ']') 
			ifTrue: [ ^ result ].
		self expect: ',' ].
	self error: 'end of array expected'! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 4/23/2012 23:39'!
parseCharacter
	| char |
	(char := stream next) = $\ 
		ifFalse: [ ^ char ].
	(char := stream next) = $" 
		ifTrue: [ ^ char ].
	char = $\
		ifTrue: [ ^ char ].
	char = $/
		ifTrue: [ ^ char ].
	char = $b
		ifTrue: [ ^ Character backspace ].
	char = $f
		ifTrue: [ ^ Character newPage ].
	char = $n
		ifTrue: [ ^ Character lfCharacter ].
	char = $r
		ifTrue: [ ^ Character crCharacter ].
	char = $t
		ifTrue: [ ^ Character tab ].
	char = $u
		ifTrue: [ ^ self parseCharacterHex ].
	self error: 'invalid escape character \' , (String with: char)! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'jmv 6/6/2012 11:57'!
parseCharacterHex
	| value |
	value := self parseCharacterHexDigit.
	3 timesRepeat: [ value := (value << 4) + self parseCharacterHexDigit ].
	^ Character unicodeCodePoint: value! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'jmv 6/6/2012 11:56'!
parseCharacterHexDigit
	| digit |
	stream atEnd ifFalse: [
		digit _ stream next asUppercase digitValue.
		"accept hex digits"
		(digit >= 0 and: [ digit < 16 ]) ifTrue: [ ^ digit ]].
	self error: 'hex-digit expected'.! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 2/16/2012 14:39:25'!
parseNumber
	| negated number |
	negated := stream peek = $-.
	negated ifTrue: [ stream next ].
	number := self parseNumberInteger.
	(stream peek = $.) ifTrue: [
		stream next. 
		number := number + self parseNumberFraction ].
	(stream peek = $e or: [ stream peek = $E ]) ifTrue: [
		stream next.
		number := number * self parseNumberExponent ].
	negated ifTrue: [ number := number negated ].
	^ self whitespace; createNumber: number! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'jmv 6/6/2012 11:57'!
parseNumberExponent
    | number negated |
    number := 0.
    negated := stream peek = $-.
    (negated or: [ stream peek = $+ ])
        ifTrue: [ stream next ].
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next digitValue) ].
    negated
        ifTrue: [ number := number negated ].
    ^ 10 raisedTo: number! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'jmv 6/6/2012 11:57'!
parseNumberFraction
    | number power |
    number := 0.
    power := 1.0.
    [ stream atEnd not and: [ stream peek isDigit ] ]
        whileTrue: [ 
            number := 10 * number + (stream next digitValue).
            power := power * 10.0 ].
    ^ number / power! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 4/23/2012 23:35'!
parseNumberInteger
    | number |
    number := 0.
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next asciiValue - 48) ].
    ^ number! !

!CypressJsonParser methodsFor: 'parsing' stamp: 'dkh 2/16/2012 14:39:25'!
parseObject
	| result |
	self expect: '{'.
	result := self createObject.
	(self match: '}')
		ifTrue: [ ^ result ].
	[ stream atEnd ] whileFalse: [
		result := self
			addProperty: self parseProperty
			to: result.
		(self match: '}')
			ifTrue: [ ^ result ].
		self expect: ',' ].
	self error: 'end of object expected'! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 2/16/2012 14:39:25'!
parseProperty
	| name value |
	name := self parseString.
	self expect: ':'.
	value := self parseValue.
	^ self createProperty: name with: value.! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: 'dkh 2/16/2012 14:39:25'!
parseString
	| result |
	self expect: '"'.
	result := WriteStream on: String new.
	[ stream atEnd or: [ stream peek = $" ] ] 
		whileFalse: [ result nextPut: self parseCharacter ].
	^ self expect: '"'; createString: result contents! !

!CypressJsonParser methodsFor: 'parsing' stamp: 'dkh 2/16/2012 14:39:25'!
parseValue
	| char |
	stream atEnd ifFalse: [ 
		char := stream peek.
		char = ${
			ifTrue: [ ^ self parseObject ].
		char = $[
			ifTrue: [ ^ self parseArray ].
		char = $"
			ifTrue: [ ^ self parseString ].
		(char = $- or: [ char between: $0 and: $9 ])
			ifTrue: [ ^ self parseNumber ].
		(self match: 'true')
			ifTrue: [ ^ self createTrue ].
		(self match: 'false')
			ifTrue: [ ^ self createFalse ].
		(self match: 'null')
			ifTrue: [ ^ self createNull ] ].
	self error: 'invalid input'! !

!CypressJsonParser methodsFor: 'private' stamp: 'dkh 2/16/2012 14:39:25'!
whitespace
	"Strip whitespaces from the input stream."

	[ stream atEnd not and: [ stream peek isSeparator ] ]
		whileTrue: [ stream next ]! !

!CypressJsonParser class methodsFor: 'instance creation' stamp: 'dkh 2/16/2012 14:39:25'!
new
	self error: 'Instantiate the parser with a stream.'! !

!CypressJsonParser class methodsFor: 'instance creation' stamp: 'dkh 2/16/2012 14:39:25'!
on: aStream
	^ self basicNew initializeOn: aStream! !

!CypressJsonParser class methodsFor: 'accessing' stamp: 'dkh 2/16/2012 14:39:25'!
parse: aString
	^ self parseStream: aString readStream! !

!CypressJsonParser class methodsFor: 'accessing' stamp: 'dkh 2/16/2012 14:39:25'!
parseStream: aStream
	^ (self on: aStream) parse! !

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

!CypressMethodStructure methodsFor: 'accessing' stamp: 'dkh 4/23/2012 23:37'!
cypressSource

	| stream |
	stream := WriteStream on: String new.
	stream 
		nextPutAll: self category;
		newLine;
		nextPutAll: self source.
	^stream contents
! !

!CypressMethodStructure methodsFor: 'private' stamp: 'dkh 4/23/2012 23:28'!
extractCypressSource: aString
    | stream categoryStream sourceStream readingCategory |
    stream := ReadStream on: aString.
    categoryStream := WriteStream on: String new.
    sourceStream := WriteStream on: String new.
    readingCategory := true.
    [ stream atEnd ]
        whileFalse: [ 
            | char |
            char := stream next.
            readingCategory
                ifTrue: [ 
                    char = Character lfCharacter
                        ifTrue: [ readingCategory := false ]
                        ifFalse: [ categoryStream nextPut: char ] ]
                ifFalse: [ sourceStream nextPut: char ] ].
    self category: categoryStream contents.
    self source: sourceStream contents! !

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

!CypressMethodStructure methodsFor: 'accessing' stamp: 'dkh 4/24/2012 00:07'!
selector
    ^ String
        streamContents: [ :stream | 
            self name
                do: [ :chara | 
                    stream
                        nextPut:
                            (chara = $.
                                ifTrue: [ $: ]
                                ifFalse: [ chara ]) ] ]! !

!CypressMethodStructure methodsFor: 'accessing' stamp: 'dkh 4/24/2012 00:06'!
selector: aString
    name := String
        streamContents: [ :stream | 
            aString
                do: [ :chara | 
                    stream
                        nextPut:
                            (chara = $:
                                ifTrue: [ $. ]
                                ifFalse: [ chara ]) ] ]! !

!CypressMethodStructure methodsFor: 'accessing'!
source

	^source
! !

!CypressMethodStructure methodsFor: 'accessing'!
source: aString

	source := aString
! !

!CypressMethodStructure methodsFor: 'writing' stamp: 'dkh 4/23/2012 23:37'!
writeJsonOn: aStream  indent: startIndent

	| indent |
	indent := startIndent.
	aStream 
		tab: indent;
		nextPutAll: '{';
		newLine.
	indent := indent + 1.
	aStream
		tab: indent;
		nextPutAll: '"name"';
		nextPutAll: ' : ';
		nextPutAll: '"', self name, '.st",';
		newLine.
	aStream
		tab: indent;
		nextPutAll: '"contents"';
		nextPutAll: ' : '.
	self cypressSource writeCypressJsonOn: aStream indent: indent.
	indent := indent - 1.
	aStream
		newLine;
		tab: indent;
		nextPutAll: ' }'
! !

!CypressMethodStructure class methodsFor: 'instance creation'!
fromMethodDefinition: methodDefinition

	^self new
		fromMethodDefinition: methodDefinition;
		yourself
! !

!CypressPackageReader methodsFor: 'private' stamp: 'jmv 6/6/2012 23:24'!
classStructureFrom: classPropertiesDict 

	^(CypressClassStructure new)
		isClassExtension: true;
		properties: classPropertiesDict;
		packageStructure: packageStructure;
		yourself! !

!CypressPackageReader methodsFor: 'private' stamp: 'dkh 4/22/2012 13:24:15'!
classStructureFrom: classPropertiesDict comment: classComment.

	^(self classStructureFrom: classPropertiesDict)
		isClassExtension: false;
		comment: classComment;
		yourself! !

!CypressPackageReader methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageDirectory

	^packageDirectory! !

!CypressPackageReader methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageDirectory: aDirectory

	packageDirectory := aDirectory! !

!CypressPackageReader methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageStructure

	^packageStructure! !

!CypressPackageReader methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageStructure: aPackageStructure

	packageStructure := aPackageStructure! !

!CypressPackageReader methodsFor: 'reading' stamp: 'dkh 4/22/2012 13:24:15'!
read

    	self readPropertiesFile.
	self readPackageStructure! !

!CypressPackageReader methodsFor: 'reading' stamp: 'dkh 4/23/2012 20:19'!
readClassStructureFromEntry: classEntry

    | classDirectory classPropertiesDict classComment entries classStructure |
    classDirectory := classEntry asFileDirectory.
    ((entries := classDirectory entries) detect: [ :entry | entry name = 'properties.json' ] ifNone: [  ])
        ifNotNil: [ :propertyEntry | propertyEntry readStreamDo: [ :fileStream | classPropertiesDict := CypressJsonParser parseStream: fileStream ] ].
    (entries detect: [ :entry | entry name = 'README.md' ] ifNone: [  ])
        ifNotNil: [ :commentEntry | commentEntry readStreamDo: [ :fileStream | classComment := fileStream contents ] ].
    classStructure := self classStructureFrom: classPropertiesDict comment: classComment.
    self readMethodStructureFor: classStructure in: entries.
	^classStructure! !

!CypressPackageReader methodsFor: 'reading' stamp: 'dkh 4/23/2012 20:20'!
readExtensionClassStructureFromEntry: classEntry

    | classDirectory classPropertiesDict entries classStructure |
    classDirectory := classEntry asFileDirectory.
    ((entries := classDirectory entries) detect: [ :entry | entry name = 'properties.json' ] ifNone: [  ])
        ifNotNil: [ :propertyEntry | propertyEntry readStreamDo: [ :fileStream | classPropertiesDict := CypressJsonParser parseStream: fileStream ] ].
    classStructure := self classStructureFrom: classPropertiesDict.
    self readMethodStructureFor: classStructure in: entries.
	^classStructure! !

!CypressPackageReader methodsFor: 'reading' stamp: 'jmv 6/6/2012 23:34'!
readMethodStructureFor: classStructure in: entries

    entries
        do: [ :entry | 
            | methods isMeta |
		isMeta := false.
 		methods := entry name = 'class'
                ifTrue: [ 
			isMeta := true.
			classStructure classMethods ]
		    ifFalse: [ classStructure instanceMethods ].
            (entry name = 'instance' or: [ entry name = 'class' ])
                ifTrue: [ 
                    (entry asFileDirectory entries select: [ :each | each name first ~= $. and: [ each name endsWith: '.st' ]])
                        do: [ :methodEntry | 
                            methodEntry
                                readStreamDo: [ :fileStream | 
                                    | category source selector |
                                    category := fileStream nextLine.
                                    source := fileStream upToEnd.
						selector := Parser new parseSelector: source.
                                     methods 
							at: selector
							put: ((CypressMethodStructure new)
									classStructure: classStructure;
									name: selector;
									isMetaclass: isMeta;
									selector: selector;
									category: category;
									source: source;
									yourself) ] ] ] ]! !

!CypressPackageReader methodsFor: 'reading' stamp: 'jmv 6/6/2012 23:06'!
readPackageStructure
	packageStructure _ CypressPackageStructure new name: self packageDirectory localName.
	self packageDirectory entries do: [ :entry |
		entry name first ~= $. ifTrue: [
			(entry name endsWith: '.class') ifTrue: [
				self packageStructure classes add: (self readClassStructureFromEntry: entry) ].
			(entry name endsWith: '.extension') ifTrue: [
				self packageStructure extensions add: (self readExtensionClassStructureFromEntry: entry) ]]]! !

!CypressPackageReader methodsFor: 'reading' stamp: 'dkh 4/23/2012 20:20'!
readPropertiesFile	

	self packageDirectory 
		readOnlyFileNamed: 'properties.json'
		do: [:fileStream |
			properties := CypressJsonParser parseStream: fileStream ]! !

!CypressPackageReader class methodsFor: 'services' stamp: 'jmv 6/13/2012 09:10'!
installAsCodePackage: aCypressPackageDirectory
	"
	For example:
		CypressPackageReader installAsCodePackage: (FileDirectory default directoryNamed: 'Cypress-Mocks.package')
	"
	| reader cypressStructure incomingSnapshot |
	reader _ CypressPackageReader readPackageStructureFrom: aCypressPackageDirectory.
	cypressStructure _ reader packageStructure.
	incomingSnapshot _ cypressStructure snapshot.
	incomingSnapshot updatePackage: (CypressPackageDefinition new name: cypressStructure packageName).
	CodePackage named: cypressStructure packageName createIfAbsent: true registerIfNew: true! !

!CypressPackageReader class methodsFor: 'instance creation' stamp: 'dkh 4/22/2012 13:24:15'!
readPackageStructureFrom: aPackagesDirectory

	^(self new)
		packageDirectory: aPackagesDirectory;
		read;
		yourself! !

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

!CypressPackageStructure methodsFor: 'initialization' stamp: 'dkh 4/24/2012 00:46'!
fromJs: jsObject

	name := jsObject at: 'name'.
	(jsObject at: 'contents') do: [:jsClassObject| | classStructure objectName |
		classStructure := (CypressClassStructure new)
                		packageStructure: self;
				yourself.
                (  '*.extension' match:(objectName := jsClassObject at: 'name') )
			ifTrue: [ 
				classStructure isClassExtension: true.
				self extensions add: classStructure ]
			ifFalse: [
				( '*.class' match: objectName)
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

!CypressPackageStructure methodsFor: 'writing' stamp: 'dkh 4/23/2012 23:39'!
writeJsonOn: aStream  indent: startIndent

	| indent |
	indent := startIndent.
	aStream 
		tab: indent;
		nextPutAll: '{';
		newLine.
	indent := indent + 1.
	aStream
		tab: indent;
		nextPutAll: '"name"';
		nextPutAll: ' : ';
		nextPutAll: '"', self name, '",'.
	aStream
		newLine;
		tab: indent;
		nextPutAll: '"contents" : [';
		newLine;
		yourself.
	1 to: self classes size do: [:index | | classStructure | 
		classStructure := self classes at: index.
		classStructure writeJsonOn: aStream indent: indent + 1.
		(self extensions size > 0 or: [ index < self classes size]) ifTrue: [ aStream nextPutAll: ','; newLine. ]].
	1 to: self extensions size do: [:index | | classStructure | 
		classStructure := self extensions at: index.
		classStructure writeJsonOn: aStream indent: indent + 1.
		index < self extensions size ifTrue: [ aStream nextPutAll: ','; newLine.] ].
	aStream
		newLine;
		tab: indent;
		nextPutAll: '],';
		newLine;
		tab: indent;
		nextPutAll: '"properties.json" : '.
	self properties writeCypressJsonOn: aStream indent: indent.
	indent := indent - 1.
	aStream 
		newLine;
		tab: indent;
		nextPutAll: '}'
! !

!CypressPackageStructure class methodsFor: 'instance creation' stamp: 'dkh 4/23/2012 23:33'!
fromJson: aJsonString

	^self fromJs: (CypressJsonParser parse: aJsonString)
! !

!CypressPackageStructure class methodsFor: 'instance creation'!
fromPackage: aCypressPackageDefinition

	^(self new) 
		fromPackage: aCypressPackageDefinition;
		yourself
! !

!CypressPackageWriter methodsFor: 'private' stamp: 'dkh 4/22/2012 13:24:15'!
directoryForDirectoryNamed: directoryNameOrPath
    ^ directoryNameOrPath = '.'
        ifTrue: [ self packageDirectory assureExistence ]
        ifFalse: [ | dir |
            dir := self packageDirectory directoryNamed: directoryNameOrPath.
            dir assureExistence.
            dir  ]! !

!CypressPackageWriter methodsFor: 'private' stamp: 'dkh 4/22/2012 13:24:15'!
fileNameForSelector: selector
    ^ selector last = $:
        ifTrue: [ 
            selector
                collect: [ :each | 
                    each = $:
                        ifTrue: [ $. ]
                        ifFalse: [ each ] ] ]
        ifFalse: [ 
            selector first isLetter
                ifTrue: [ selector ]
                ifFalse: [ 
                    | output specials |
                    specials := self class specials.
                    output := String new writeStream.
                    output nextPut: $^.
                    selector do: [ :each | output nextPutAll: (specials at: each) ] separatedBy: [ output nextPut: $. ].
                    output contents ] ]! !

!CypressPackageWriter methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageDirectory

	packageDirectory 
		ifNil: [ 
			packageDirectory := self rootDirectory directoryNamed: self packageStructure name.
			packageDirectory assureExistence ].
	^packageDirectory! !

!CypressPackageWriter methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageDirectory: aPackageDirectory

	packageDirectory := aPackageDirectory! !

!CypressPackageWriter methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageStructure

	^packageStructure! !

!CypressPackageWriter methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
packageStructure: aCypressPackageStructure

	packageStructure := aCypressPackageStructure! !

!CypressPackageWriter methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
rootDirectory

	^rootDirectory! !

!CypressPackageWriter methodsFor: 'accessing' stamp: 'dkh 4/22/2012 13:24:15'!
rootDirectory: aDirectory

	rootDirectory := aDirectory! !

!CypressPackageWriter methodsFor: 'writing' stamp: 'dkh 4/22/2012 13:24:15'!
write

	self packageDirectory exists
        ifTrue: [ self packageDirectory recursiveDelete ].
    	self writePropertiesFile.
	self writePackageStructure! !

!CypressPackageWriter methodsFor: 'writing' stamp: 'jmv 5/12/2012 19:53'!
writeClassComment: classStructure on: fileStream

    fileStream nextPutAll: (classStructure comment withLineEndings: String lfString)! !

!CypressPackageWriter methodsFor: 'writing' stamp: 'jmv 5/12/2012 19:54'!
writeClassStructure: classStructure on: fileStream

    | properties |
    properties := Dictionary new.
    properties at: 'name' put: classStructure className.
    properties at: 'super' put: classStructure superclassName.
    properties at: 'instvars' put: classStructure instanceVariableNames.
    properties at: 'classinstvars' put: classStructure classInstanceVariableNames.
    properties writeCypressJsonOn: fileStream  indent: 0! !

!CypressPackageWriter methodsFor: 'writing' stamp: 'dkh 4/22/2012 13:24:15'!
writeClassStructure: classStructure to: classPath

    self
        writeInDirectoryName: classPath
        fileName: 'README'
        extension: '.md'
        visit: [:fileStream | self writeClassComment: classStructure on: fileStream ].
    self
        writeInDirectoryName: classPath
        fileName: 'properties'
        extension: '.json'
        visit: [:fileStream | self writeClassStructure: classStructure on: fileStream ]! !

!CypressPackageWriter methodsFor: 'writing' stamp: 'jmv 5/12/2012 19:54'!
writeExtensionClassStructure: classStructure to: classPath

     self
        writeInDirectoryName: classPath
        fileName: 'properties'
        extension: '.json'
        visit: [:fileStream |  | properties |
    		properties := Dictionary new.
    		properties at: 'name' put: classStructure className.
    		properties writeCypressJsonOn: fileStream  indent: 0 ]! !

!CypressPackageWriter methodsFor: 'private' stamp: 'jmv 6/6/2012 12:03'!
writeInDirectoryName: directoryNameOrPath fileName: fileName extension: ext visit: visitBlock
    | directory |
    directory := self directoryForDirectoryNamed: directoryNameOrPath.
    directory
        forceNewFileNamed: fileName , ext
        do: [ :file |
            visitBlock value: file ]! !

!CypressPackageWriter methodsFor: 'writing' stamp: 'jmv 5/12/2012 19:53'!
writeMethodStructure: methodStructure to:methodPath

    | filename |
    filename := self fileNameForSelector: methodStructure selector.
    self
        writeInDirectoryName: methodPath
        fileName: filename
        extension: '.st'
        visit: [:fileStream |
		fileStream
        		nextPutAll: methodStructure category;
        		newLine;
        		nextPutAll: (methodStructure source withLineEndings: String lfString) ]! !

!CypressPackageWriter methodsFor: 'writing' stamp: 'dkh 4/22/2012 13:24:15'!
writePackageStructure

	self writePackageStructureClasses:  self packageStructure classes isClassExtension: false.
	self writePackageStructureClasses:  self packageStructure extensions isClassExtension: true
! !

!CypressPackageWriter methodsFor: 'writing' stamp: 'dkh 4/22/2012 13:24:15'!
writePackageStructureClasses:  classStructures isClassExtension: isClassExtension
    | classDirExtension |
	
    classDirExtension := isClassExtension
		ifTrue: [ '.extension' ]
		ifFalse: [ '.class' ].
    classStructures
        do: [ :classStructure | 
            | classPath instanceMethodPath classMethodPath |
            classPath := classStructure className , classDirExtension , FileDirectory slash.
	      isClassExtension
			ifTrue: [ self writeExtensionClassStructure: classStructure to: classPath ]
            	ifFalse: [ self writeClassStructure: classStructure to: classPath ].
            instanceMethodPath := classPath , 'instance' , FileDirectory slash.
            classStructure instanceMethods
                do: [ :methodStructure |  self writeMethodStructure: methodStructure to: instanceMethodPath ].
            classMethodPath := classPath , 'class' , FileDirectory slash.
            classStructure classMethods
                do: [ :methodStructure |  self writeMethodStructure: methodStructure to: classMethodPath ] ].
! !

!CypressPackageWriter methodsFor: 'writing' stamp: 'jmv 5/12/2012 19:54'!
writePropertiesFile

    self
        writeInDirectoryName: '.'
        fileName: 'properties'
        extension: '.json'
        visit: [:fileStream | Dictionary new writeCypressJsonOn: fileStream indent: 0 ]! !

!CypressPackageWriter class methodsFor: 'as yet unclassified' stamp: 'dkh 4/22/2012 13:24:15'!
initializeSpecials
    | map |
    map := Dictionary new.
    map
        at: $+ put: 'plus';
        at: $- put: 'minus';
        at: $= put: 'equals';
        at: $< put: 'less';
        at: $> put: 'more';
        at: $% put: 'percent';
        at: $& put: 'and';
        at: $| put: 'pipe';
        at: $* put: 'star';
        at: $/ put: 'slash';
        at: $\ put: 'backslash';
        at: $~ put: 'tilde';
        at: $? put: 'wat';
        at: $@ put: 'at'.
    map keys do: [ :key | map at: (map at: key) put: key ].
    ^ map! !

!CypressPackageWriter class methodsFor: 'as yet unclassified' stamp: 'dkh 4/22/2012 13:24:15'!
specials
    ^ specials ifNil: [ specials := self initializeSpecials ]! !

!CypressPackageWriter class methodsFor: 'services' stamp: 'jmv 6/13/2012 09:06'!
writeCodePackage: aCodePackage
	"
	For example:
		CypressPackageWriter writeCodePackage: (CodePackage named: 'Cypress-Structure' createIfAbsent: true registerIfNew: false)
		CypressPackageWriter writeCodePackage: (CodePackage named: 'Morphic' createIfAbsent: true registerIfNew: false)
	"
	CypressPackageWriter
		writePackageStructure: 
			(CypressPackageStructure fromPackage: 
				(CypressPackageDefinition new name: aCodePackage packageName))
		to: FileDirectory default! !

!CypressPackageWriter class methodsFor: 'instance creation' stamp: 'dkh 4/22/2012 13:24:15'!
writePackageStructure: aPackageStructure to: aPackagesDirectory

	self new
		packageStructure: aPackageStructure;
		rootDirectory: aPackagesDirectory;
		write! !

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

!Dictionary methodsFor: '*Cypress-Structure' stamp: 'dkh 4/23/2012 23:54'!
asCypressPropertyObject
    self associations do: [ :assoc | self at: assoc key put: assoc value asCypressPropertyObject ]! !

!Dictionary methodsFor: '*Cypress-Structure' stamp: 'dkh 4/23/2012 23:54'!
writeCypressJsonOn: aStream indent: startIndent
    | indent count |
    indent := startIndent.
    aStream
        nextPutAll: '{';
        newLine.
    count := 0.
    indent := indent + 1.
    (self keys sort: [ :a :b | a <= b ])
        do: [ :key | 
            | value |
            value := self at: key.
            count := count + 1.
            aStream tab: indent.
            key writeCypressJsonOn: aStream indent: indent.
            aStream nextPutAll: ' : '.
            value writeCypressJsonOn: aStream indent: indent.
            count < self size
                ifTrue: [ 
                    aStream
                        nextPutAll: ',';
                        newLine ] ].
    self size = 0
        ifTrue: [ aStream tab: indent ].
    aStream nextPutAll: ' }'! !

!Number methodsFor: '*Cypress-Structure'!
writeCypressJsonOn: aStream  indent: startIndent

	aStream 
		nextPutAll: self printString
! !

!Object methodsFor: '*Cypress-Structure'!
asCypressPropertyObject

	^self
! !

!String methodsFor: '*Cypress-Structure' stamp: 'dkh 4/23/2012 23:49'!
asCypressPropertyObject

	^self unescapePercents withLineEndings: String lfString
! !

!String methodsFor: '*Cypress-Structure' stamp: 'dkh 4/23/2012 23:47'!
encodeForHTTP
	"change dangerous characters to their %XX form, for use in HTTP transactions"
	| encodedStream |
	encodedStream _ WriteStream on: (String new).
	
	1 to: self size do: [ :n | | c |
		c := self at: n.
		c isSafeForHTTP ifTrue: [ encodedStream nextPut: c ] ifFalse: [
			encodedStream nextPut: $%.
			encodedStream nextPut: (c asciiValue // 16) asHexDigit.
			encodedStream nextPut: (c asciiValue \\ 16) asHexDigit.
		]
	].
	^encodedStream contents.! !

!String methodsFor: '*Cypress-Structure' stamp: 'dkh 4/23/2012 23:48'!
writeCypressJsonOn: aStream  indent: startIndent

	aStream 
		nextPutAll: '"';
		nextPutAll: (self withLineEndings: String lfString) encodeForHTTP;
		nextPutAll: '"'
! !
