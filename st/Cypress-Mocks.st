!classDefinition: #CypressMockBasic category: #'Cypress-Mocks'!
Object subclass: #CypressMockBasic
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Mocks'!

!CypressMockBasic methodsFor: 'accessing'!
extra
	"extra method"
! !

!CypressMockBasic methodsFor: 'accessing'!
name
	^name
! !

!CypressMockBasic methodsFor: 'accessing'!
name: aString
	name := aString
! !


!CypressMockBasic methodsFor: 'initialization'!
initialize
	super initialize.
	self name: 'Unknown'
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CypressMockBasic class' category: #'Cypress-Mocks'!
CypressMockBasic class
	instanceVariableNames: 'current'!

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

CypressMockBasic initialize!