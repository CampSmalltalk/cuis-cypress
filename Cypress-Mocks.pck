'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 24 April 2012 at 12:36:08 am'!
'Description Please enter a description for this package '!
!classDefinition: #CypressMockBasic category: #'Cypress-Mocks'!
Object subclass: #CypressMockBasic
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Mocks'!
!classDefinition: 'CypressMockBasic class' category: #'Cypress-Mocks'!
CypressMockBasic class
	instanceVariableNames: 'current'!


!CypressMockBasic commentStamp: 'dkh 4/24/2012 00:20' prior: 0!
## Class Comment

This mock contains basic class and instance method selectors.

 [**GitHub Flabored Markdown**][1] with **Smalltalk** sytax *highlighting*:

```Smalltalk
initialize
	super initialize.
	self name: 'Unknown'
```

And some [UTF8 samples][2]: 

```
UTF8 not handled correctly
```


[1]: http://github.github.com/github-flavored-markdown/
[2]: http://www.columbia.edu/~fdc/utf8/!

!CypressMockBasic methodsFor: 'accessing' stamp: 'dkh 4/24/2012 00:22'!
extra
	"extra method"! !

!CypressMockBasic methodsFor: 'initialization' stamp: 'dkh 4/24/2012 00:33'!
initialize
	super initialize.
	self name: 'Unknown'! !

!CypressMockBasic methodsFor: 'accessing' stamp: 'dkh 4/24/2012 00:22'!
name
	^name! !

!CypressMockBasic methodsFor: 'accessing' stamp: 'dkh 4/24/2012 00:23'!
name: aString
	name := aString! !

!CypressMockBasic class methodsFor: 'accessing' stamp: 'dkh 4/24/2012 00:23'!
current
	^current! !

!CypressMockBasic class methodsFor: 'accessing' stamp: 'dkh 4/24/2012 00:23'!
current: anObject
	current := anObject! !

!CypressMockBasic class methodsFor: 'initialization' stamp: 'dkh 4/24/2012 00:23'!
initialize
	self current: self new! !

!Object methodsFor: '*Cypress-Mocks' stamp: 'dkh 4/24/2012 00:35'!
isCypressMockBasic
	^false! !
CypressMockBasic initialize!
