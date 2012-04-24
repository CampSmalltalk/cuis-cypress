'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 23 April 2012 at 8:40:41 pm'!
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
