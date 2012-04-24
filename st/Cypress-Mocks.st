Object subclass: #CypressMockBasic
	instanceVariableNames: 'name'
classVariableNames: '' poolDictionaries: '' category: 'Cypress-Mocks'!

!CypressMockBasic methodsFor: 'accessing'!

extra
	"extra method"
!

name
	^name
!

name: aString
	name := aString
! !

!CypressMockBasic methodsFor: 'initialization'!

initialize
	super initialize.
	self name: 'Unknown'
! !

CypressMockBasic class instanceVariableNames: 'current'!

!CypressMockBasic class methodsFor: 'accessing'!

current
	^current
!

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

