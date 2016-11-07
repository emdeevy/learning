.data
.align 2

input:  .asciiz "\nEnter 1st int: "
input2: .asciiz "\nEnter 2nd int: "

.text
.globl main

main:

    li $v0, 4                   ; # System call code 4 for print string
    la $a0, input               ; # Argument string as input
    syscall                     ; # Print the string
    
    li $v0, 5                   ; # System call code 5 to read int input
    syscall                     ; # Read it
    move $a1, $v0               ; # Move 1st int into a1
        
    li $v0, 4                   ; # System call code 4 for print string
    la $a0, input2              ; # Argument string as input
    syscall                     ; # Print the string
    
    li $v0, 5                   ; # System call code 5 to read int input
    syscall                     ; # Read it
    move $a2, $v0               ; # Move 2nd int into a2

    jal gcd                     ; # Jump to gcd ( do the job and expect to come back )
    
    move $a0, $a1               ; # Move final answer to a0
    li $v0,1                    ; # Set final answer to be printed
    syscall                     ; # Print final answer

    li $v0, 10                  ; # System call code for exit
    syscall                     ; # exit

    gcd:

        rem $t0, $a1, $a2       ; # t0 = a1 % a2
        
        move $a1,$a2            ; # a1 <- a2
        move $a2,$t0            ; # a2 <- t0
        
        bgtz $a2,REC            ; # if( a2 > 0 ) { REC }
        
        j $ra                   ; # Jump back to jal

    REC:    
    
        subi $sp, $sp, 4        ; # Make room for 1 position
        sw $ra, 0($sp)          ; # save on 1st position in stack
        
        j gcd
        