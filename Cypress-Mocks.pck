'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 23 April 2012 at 9:34:01 pm'!
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


!CypressMockBasic commentStamp: 'dkh 4/23/2012 21:31' prior: 0!
## Class Comment

This mock contains basic class and instance method selectors.

 [**GitHub Flabored Markdown**][1] with **Smalltalk** sytax *highlighting*:

```Smalltalk
initialize
	super initialize.
	self name: ''Unknown''
```

And some [UTF8 samples][2]: 

```
  Lithuanian: Aš galiu valgyti stiklą ir jis manęs nežeidžia
  Russian: Я могу есть стекло, оно мне не вредит.
  Korean: 나는 유리를 먹을 수 있어요. 그래도 아프지 않아요
  Hebrew: אני יכול לאכול זכוכית וזה לא מזיק לי.
  Latin: Vitrum edere possum; mihi non nocet.
```


[1]: http://github.github.com/github-flavored-markdown/
[2]: http://www.columbia.edu/~fdc/utf8/!

!CypressMockBasic methodsFor: 'accessing'!
extra
	"extra method"
! !

!CypressMockBasic methodsFor: 'initialization'!
initialize
	super initialize.
	self name: 'Unknown'
! !

!CypressMockBasic methodsFor: 'accessing'!
name
	^name
! !

!CypressMockBasic methodsFor: 'accessing'!
name: aString
	name := aString
! !

!CypressMockBasic class methodsFor: 'accessing'!
current
	^current
! !

!CypressMockBasic class methodsFor: 'accessing'!
current: anObject
	current := anObject
! !

!CypressMockBasic class methodsFor: 'initialization'!
initialize
	self current: self new
! !

!Object methodsFor: '*Cypress-Mocks'!
isCypressMockBasic
	^false
! !
CypressMockBasic initialize!
