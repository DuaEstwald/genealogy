! Author: ELENA ARJONA GALVEZ


module tree_mod
    implicit none

    type person_ptr
        type(person_type), pointer :: ptr => null()
    end type person_ptr
    type person_type
        integer :: id
        type(person_type), pointer :: left => null()
        type(person_type), pointer :: right => null()
        type(person_ptr) :: parent(4)
        type(person_ptr) :: child(4)
    end type person_type

contains

    recursive subroutine re_tree_insert(pC,pN)
    ! This subroutine inserts a new (allocated) node into a tree starting from root.
        implicit none
        type(person_type), pointer, intent(inout):: pC, pN


        if (associated(pC)) then
            if (pN%id < pC%id) then
                call re_tree_insert(pC%left,pN)

            else if (pN%id > pC%id) then
                call re_tree_insert(pC%right,pN)

            else
                deallocate(pN)
                pN => pC
            end if
        else
            pC => pN
            print '(a, i0, a)', "Node ", pN%id, " has been created."
        end if

    end subroutine re_tree_insert


    recursive subroutine tree_remove(pN)
    ! This subroutine, delete the tree node by node, pointed by root node.
        implicit none
        type(person_type), pointer, intent(inout) :: pN


        if (associated(pN)) then
                call tree_remove(pN%left)
                call tree_remove(pN%right)
                print '(a, i0, a)', "Node ", pN%id, " has been remove."
                deallocate(pN)
        end if

    end subroutine tree_remove
       

    recursive subroutine tree_print(pN)
    ! This subroutine print the values of the tree in the inorder tree walk: 
    !left, value, right. Thus, the values are always printed in the ascending order. 


        implicit none
        type(person_type), pointer, intent(in) :: pN
        integer :: i,j


        if (associated(pN)) then
            call tree_print(pN%left)
            write(*, '(a,i0,a)') "Person ", pN%id, ":"

            do i=1,4
                if (associated(pN%parent(i)%ptr)) then
                    write(*,'(a,i0)') "   parent ", pN%parent(i)%ptr%id
                else
                    continue
                end if
            end do
            do j = 1,4
                if (associated(pN%child(j)%ptr)) then
                    write(*,'(a,i0)') "   child ", pN%child(j)%ptr%id
                else
                    exit
                end if                           
            end do
            
            call tree_print(pN%right)
            
        end if

    end subroutine tree_print



    subroutine tree_relation(p1, p2, rel)
    ! This subroutine insert the relations between each node.
        implicit none
        type(person_type), pointer, intent(in):: p1, p2
        type(person_ptr), pointer :: crnt(:)
        character(len=6), intent(in) :: rel
        integer :: i

        if (rel == 'child') crnt => p1%child
        if (rel == 'parent') crnt => p1%parent
        do i =1,5 
 
            if (i == 5) then
                print '(a,i0,2(a),a,i0,a)', "Warning: ", p1%id, " has no ",&
                     trim(rel), " left for ", p2%id, "."
                exit
            end if 

                            
            if (associated(crnt(i)%ptr,p2)) then
                print '(a,i0,2(a),a,i0,a)', "Warning: ", p1%id, &
                        " already has a ", trim(rel), " for ", p2%id, "."
                exit
            else if (.not.associated(crnt(i)%ptr)) then
                crnt(i)%ptr => p2
                 
                exit
            end if
       
        end do
    end subroutine tree_relation

end module tree_mod


program genealogy    
    use tree_mod, only: person_type, person_ptr, re_tree_insert, tree_remove,&
        tree_print, tree_relation    
    implicit none
    
    character(len=6) :: relation
    character(len=3) :: no
    integer :: id1, id2
    integer :: iostatus

    type(person_type), pointer :: root => null()
    type(person_type), pointer :: tmp1 => null()
    type(person_type), pointer :: tmp2 => null()
    
    ! Read and create the tree.
    do  
        read (*,*,iostat = iostatus) id1, no, relation, id2
        if (iostatus /= 0) exit
        if ((relation /= 'parent') .and. (relation /= 'child')) then
            print '(3(a))', "Warning: '", trim(relation), "' is not a legal relation."
            
        else 
            allocate(tmp1,tmp2)
            tmp1%id = id1
            tmp2%id = id2
            call re_tree_insert(root,tmp1)
            call re_tree_insert(root,tmp2)
            call tree_relation(tmp1,tmp2, relation)

        end if
        
    end do
    call tree_print(root)
    call tree_remove(root)
end program genealogy
