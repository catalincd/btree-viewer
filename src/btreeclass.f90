module btreeclass

    use btreenode
    use M_strings
    implicit none

    type BTree
        integer :: T = 2
        integer :: MAX_KEYS = 2 * 2 - 1
        integer :: MAX_CHILDREN = 2 * 2
        type(BNodePointer) :: root

    end type BTree
contains
    subroutine Insert(this, value)
        type(BTree), intent(inout) :: this
        integer :: value

        type(BNodePointer) :: root_copy
        type(BNodePointer) :: new_root

        if(this%root%IsNull) then
            allocate(this%root%addr)
            call Init(this%root%addr)
            this%root%IsNull = .false.
            this%root%addr%Leaf = .true.
        end if

        if(this%root%addr%KeysNum == this%MAX_KEYS) then
            root_copy = this%root
            root_copy%IsNull = .false.

            allocate(new_root%addr)
            call Init(new_root%addr)
            new_root%IsNull = .false.

            this%root = new_root
            new_root%addr%ChildrenNum = 1
            new_root%addr%Children(0) = root_copy

            call SplitChild(this, new_root, 0)
        end if

        call InsertNonFull(this, this%root, value)

    end subroutine Insert

    subroutine SplitChild(this, node, i)
        type(BTree), intent(inout) :: this
        type(BNodePointer), intent(inout) :: node
        integer :: i
        integer :: T
        integer :: q

        type(BNodePointer) :: y 
        type(BNodePointer) :: z

        T = this%T

        y = node%addr%Children(i)

        allocate(z%addr)
        call Init(z%addr)
        z%IsNull = .false.
        z%addr%Leaf = y%addr%Leaf

        do q=node%addr%KeysNum,i+1,-1
            node%addr%Keys(q) = node%addr%Keys(q-1)
        end do
        node%addr%Keys(i) = y%addr%Keys(T - 1)

        do q=node%addr%KeysNum,i+1,-1
            node%addr%Children(q+1) = node%addr%Children(q)
        end do

        node%addr%Children(i+1) = z;
        node%addr%ChildrenNum = node%addr%ChildrenNum + 1

        do q=T,this%MAX_KEYS-1
            z%addr%Keys(q-T) = y%addr%Keys(q)
        end do

        node%addr%KeysNum = node%addr%KeysNum + 1
        z%addr%KeysNum = T-1
        y%addr%KeysNum = T-1


        if(.not. y%addr%Leaf) then
            do q=T,this%MAX_CHILDREN-1
                z%addr%Children(q-T) = y%addr%Children(q)
            end do

            y%addr%ChildrenNum = T
            z%addr%ChildrenNum = T
        end if


    end subroutine SplitChild


    recursive subroutine InsertNonFull(this, node, value)
        type(BTree), intent(inout) :: this
        type(BNodePointer), intent(inout) :: node
        integer :: value
        integer :: i
        integer :: q

        if(node%addr%Leaf) then
            i = 0
            do while(node%addr%Keys(i) < value .and. i < node%addr%KeysNum)
                i = i+1
            end do

            !print *, "END I:", i

            print *, "TRYING KEYSNUM:", node%addr%KeysNum

            if(node%addr%KeysNum > this%MAX_CHILDREN .or. node%addr%KeysNum < 0) then
                print *,"FAILED: ", node%addr%KeysNum
            end if

            do q=node%addr%KeysNum,i+1,-1
                node%addr%Keys(q) = node%addr%Keys(q-1)
            end do

            node%addr%Keys(i) = value
            node%addr%KeysNum = node%addr%KeysNum+1
            return
        end if

        do i=0,node%addr%KeysNum-1
            if(node%addr%Keys(i) > value) then
                exit
            end if
        end do

        if(node%addr%Children(i)%addr%KeysNum == this%MAX_KEYS) then
            call SplitChild(this, node, i)

            if(value > node%addr%Keys(i)) then
                i = i+1
            end if
        end if

        call InsertNonFull(this, node%addr%Children(i), value)
    
    end subroutine InsertNonFull

    subroutine Search(this, value)
        type(BTree), intent(inout) :: this
        integer, intent(in) :: value
        
        if(.not. this%root%IsNull) then
            call SearchRec(this, this%root%addr, value)
        end if 
    end subroutine Search

    recursive subroutine SearchRec(this, node, value)
        type(BTree), intent(inout) :: this
        type(BNode), intent(inout) :: node
        integer, intent(in) :: value
        integer :: i 

        do i=0,node%KeysNum-1
            if(node%Keys(i) == value) then
                node%Found = .true.
                node%FoundIdx = i
                return
            end if

            if(node%Keys(i) > value) then
                if(.not. node%Children(i)%IsNull) then
                    call SearchRec(this, node%Children(i)%addr, value)
                end if
                return
            end if
        end do

        call SearchRec(this, node%Children(node%KeysNum)%addr, value)
                

    end subroutine SearchRec

    subroutine Delete(this, value)
        type(BTree), intent(inout) :: this
        integer, intent(in) :: value

        if(.not. this%root%IsNull) then
            call DeleteRec(this, this%root, value)
        end if
    end subroutine Delete
    

    recursive subroutine DeleteRec(this, node, value)
        type(BTree), intent(inout) :: this
        type(BNodePointer), intent(inout) :: node
        integer, intent(in) :: value
        integer :: i
        integer :: q
        integer :: T 
        T = this%T

        i = 0
        do while(node%addr%Keys(i) < value .and. i < node%addr%KeysNum)
            i = i+1
        end do

        if(node%addr%Leaf) then
            if(i < node%addr%KeysNum .and. node%addr%Keys(i) == value) then
                do q=i,node%addr%KeysNum-1
                    node%addr%Keys(q) = node%addr%Keys(q+1)
                end do
                node%addr%KeysNum = node%addr%KeysNum-1
            end if
            return
        end if

        if(i < node%addr%KeysNum .and. node%addr%Keys(i) == value) then
            call DeleteInternalNode(this, node, value, i)
        else
            if(node%addr%Children(i)%addr%KeysNum >= T) then
                call DeleteRec(this, node%addr%Children(i), value)
            else
                if(i /= 0 .and. i+1 < node%addr%KeysNum) then
                    if(node%addr%Children(i-1)%addr%KeysNum >= T) then
                        call DeleteSibling(this, node, i, i-1)
                    else
                        if(node%addr%Children(i+1)%addr%KeysNum >= T) then
                            call DeleteSibling(this, node, i, i+1)
                        else
                            call DeleteMerge(this, node, i, i+1)
                        end if
                    end if  
                else
                    if(i==0) then
                        if(node%addr%Children(i+1)%addr%KeysNum >= T) then
                            call DeleteSibling(this, node, i, i+1)
                        else 
                            call DeleteMerge(this, node, i, i+1)
                        end if
                    else 
                        if(i==node%addr%KeysNum) then
                            if(node%addr%Children(i-1)%addr%KeysNum >= T) then
                                call DeleteSibling(this, node, i, i-1)
                            else
                                call DeleteMerge(this, node, i, i-1)
                            end if
                        end if
                    end if
                end if
                !what the fuck
                call DeleteRec(this, node%addr%Children(i), value)
            end if
        end if
        

    end subroutine DeleteRec

    recursive subroutine DeleteInternalNode(this, node, value, i)
        type(BTree), intent(inout) :: this
        type(BNodePointer), intent(inout) :: node
        integer, intent(in) :: value, i
        integer :: q 
        integer :: T 

        T = this%t

        if(node%addr%Leaf) then
            if(node%addr%Keys(i) == value) then
                do q=i,node%addr%KeysNum-1
                    node%addr%Keys(q) = node%addr%Keys(q+1)
                end do
                node%addr%KeysNum = node%addr%KeysNum-1
            end if
            return
        end if

        if(node%addr%Children(i)%addr%KeysNum >= T) then
            node%addr%Keys(i) = DeletePredecessor(this, node%addr%Children(i))
            return
        else
            if(node%addr%Children(i+1)%addr%KeysNum >= T) then
                node%addr%Keys(i) = DeleteSuccessor(this, node%addr%Children(i+1))
                return
            else
                call DeleteMerge(this, node, i, i+1)
                call DeleteInternalNode(this, node%addr%Children(i), value, T-1)
            end if
        end if 

    end subroutine DeleteInternalNode

    recursive function DeletePredecessor(this, node) result(key)
        type(BTree), intent(inout) :: this
        type(BNodePointer), intent(inout) :: node
        integer :: key
        integer :: n
        integer :: T 

        T = this%T

        if(node%addr%Leaf) then
            key = node%addr%Keys(node%addr%KeysNum-1)
            node%addr%KeysNum = node%addr%KeysNum - 1;
            return
        end if

        n = node%addr%KeysNum - 1

        if(node%addr%Children(n)%addr%KeysNum >= T) then
            call DeleteSibling(this, node, n+1, n)
        else
            call DeleteMerge(this, node, n, n+1)
        end if

        !what the actual
        key = DeletePredecessor(this, node%addr%Children(n))

    end function DeletePredecessor

    recursive function DeleteSuccessor(this, node) result(key)
        type(BTree), intent(inout) :: this
        type(BNodePointer), intent(inout) :: node
        integer :: key
        integer q, n
        integer :: T 

        T = this%T

        if(node%addr%Leaf) then
            key = node%addr%Keys(0)

            do q=1,node%addr%KeysNum-1
                node%addr%Keys(q-1) = node%addr%Keys(q)
            end do
            node%addr%KeysNum = node%addr%KeysNum-1
            return
        end if

        n = node%addr%KeysNum-1

        if(node%addr%Children(1)%addr%KeysNum >= T) then
        call DeleteSibling(this, node, 0, 1)
        else
            call DeleteMerge(this, node, 0, 1)
        end if

        !what the actual
        key = DeletePredecessor(this, node%addr%Children(0))

    end function DeleteSuccessor

    subroutine DeleteSibling(this, node, i, j)
        type(BTree), intent(inout) :: this
        type(BNodePointer), intent(inout) :: node
        integer, intent(in) :: i, j
        integer :: q

        type(BNodePointer) cnode, rsnode, lsnode

        cnode = node%addr%Children(i)

        if(i < j) then
            rsnode = node%addr%Children(i)
            cnode%addr%Keys(cnode%addr%KeysNum) = node%addr%Keys(i)
            cnode%addr%KeysNum = cnode%addr%KeysNum + 1
            node%addr%Keys(i) = rsnode%addr%Keys(0)

            if(.not. rsnode%addr%Leaf) then
                cnode%addr%Children(cnode%addr%ChildrenNum) = rsnode%addr%Children(0)
                cnode%addr%ChildrenNum = cnode%addr%ChildrenNum + 1

                do q=1,rsnode%addr%ChildrenNum-1
                    rsnode%addr%Children(q-1) = rsnode%addr%Children(q)
                end do

                rsnode%addr%ChildrenNum = rsnode%addr%ChildrenNum-1
            end if

            do q=1,rsnode%addr%KeysNum-1
                rsnode%addr%Keys(q-1) = rsnode%addr%Keys(q)
            end do

            rsnode%addr%KeysNum = rsnode%addr%KeysNum-1
        else
            lsnode = node%addr%Children(j)

            do q=cnode%addr%KeysNum,1,-1
                cnode%addr%Keys(q) = cnode%addr%Keys(q-1)
            end do

            cnode%addr%Keys(0) = node%addr%Keys(i-1)
            lsnode%addr%KeysNum = lsnode%addr%KeysNum-1
            node%addr%Keys(i-1) = lsnode%addr%Keys(lsnode%addr%KeysNum)

            if(lsnode%addr%ChildrenNum > 0) then
                do q=cnode%addr%ChildrenNum,1,-1
                    cnode%addr%Children(q) = cnode%addr%Children(q-1)
                end do
                lsnode%addr%ChildrenNum = lsnode%addr%ChildrenNum-1
                cnode%addr%Children(0) = lsnode%addr%Children(lsnode%addr%ChildrenNum)
            end if
        end if

    end subroutine DeleteSibling

    subroutine DeleteMerge(this, node, i, j)
        type(BTree), intent(inout) :: this
        type(BNodePointer), intent(inout) :: node
        integer, intent(in) :: i, j
        integer :: q, k

        type(BNodePointer) cnode, rsnode, lsnode, new_node

        if(j > i) then
            rsnode = node%addr%Children(j)
            cnode%addr%Keys(cnode%addr%KeysNum) = rsnode%addr%Keys(i)
            cnode%addr%KeysNum = cnode%addr%KeysNum + 1

            do k=0,rsnode%addr%KeysNum-1
                cnode%addr%Keys(cnode%addr%KeysNum) = rsnode%addr%Keys(k)
                cnode%addr%KeysNum = cnode%addr%KeysNum + 1

                if(rsnode%addr%ChildrenNum > 0) then
                    cnode%addr%Children(cnode%addr%ChildrenNum) = rsnode%addr%Children(k)
                    cnode%addr%ChildrenNum = cnode%addr%ChildrenNum + 1
                end if
            end do

            if(rsnode%addr%ChildrenNum > 0) then
                rsnode%addr%ChildrenNum = rsnode%addr%ChildrenNum - 1
                cnode%addr%Children(cnode%addr%ChildrenNum) = rsnode%addr%Children(rsnode%addr%ChildrenNum)
                cnode%addr%ChildrenNum = cnode%addr%ChildrenNum + 1
            end if

            new_node = cnode

            do q=i,node%addr%KeysNum-1
                node%addr%Keys(q) = node%addr%Keys(q+1)
            end do

            node%addr%KeysNum = node%addr%KeysNum - 1

            do q=j,node%addr%ChildrenNum-1
                node%addr%Children(q) = node%addr%Children(q+1)
            end do

            node%addr%ChildrenNum = node%addr%ChildrenNum - 1

        else

            lsnode = node%addr%Children(j)
            lsnode%addr%Keys(lsnode%addr%KeysNum) = node%addr%Keys(j)
            lsnode%addr%KeysNum = lsnode%addr%KeysNum + 1

            do q=0,cnode%addr%KeysNum-1
                lsnode%addr%Keys(lsnode%addr%KeysNum) = cnode%addr%Keys(q)
                lsnode%addr%KeysNum = lsnode%addr%KeysNum + 1

                if(lsnode%addr%ChildrenNum > 0) then
                    cnode%addr%ChildrenNum = cnode%addr%ChildrenNum - 1
                    lsnode%addr%Children(lsnode%addr%ChildrenNum) = cnode%addr%Children(cnode%addr%ChildrenNum)
                    lsnode%addr%ChildrenNum = lsnode%addr%ChildrenNum + 1
                end if
            end do

            if(lsnode%addr%ChildrenNum > 0) then
                cnode%addr%ChildrenNum = cnode%addr%ChildrenNum - 1
                lsnode%addr%Children(lsnode%addr%ChildrenNum) = cnode%addr%Children(cnode%addr%ChildrenNum)
                lsnode%addr%ChildrenNum = lsnode%addr%ChildrenNum + 1
            end if

            new_node = lsnode

            do q=j,node%addr%KeysNum-1
                node%addr%Keys(q) = node%addr%Keys(q+1)
            end do

            node%addr%KeysNum = node%addr%KeysNum-1

            do q=i,node%addr%ChildrenNum-1
                node%addr%Children(q) = node%addr%Children(q+1)
            end do

            node%addr%ChildrenNum = node%addr%ChildrenNum-1   

            if(loc(node%addr) ==  loc(this%root%addr) .and. node%addr%KeysNum == 0) then
                this%root = new_node         
            end if

        end if



    end subroutine DeleteMerge



    subroutine Print(this, str)
        type(BTree), intent(inout) :: this
        character(200), intent(out) :: str
        str = ""
        call PrintRec(this, this%root, str)

        
    end subroutine Print

    recursive subroutine PrintRec(this, node, str)
        type(BTree), intent(inout) :: this
        type(BNodePointer), intent(inout) :: node
        character(200), intent(inout) :: str
        character(50) :: temp
        integer :: i
        
        do i=0,node%addr%KeysNum-1
            if(.not. node%addr%Leaf) then
                call PrintRec(this, node%addr%Children(i), str)
            end if

            call value_to_string(node%addr%Keys(i), temp)
            str = TRIM(str)//" "//temp
        end do

        if(.not. node%addr%Leaf) then
            call PrintRec(this, node%addr%Children(node%addr%KeysNum), str)
        end if

    end subroutine PrintRec

end module btreeclass