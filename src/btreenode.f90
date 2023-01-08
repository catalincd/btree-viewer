module btreenode

    implicit none

    type BNodePointer
        type(BNode), pointer :: addr
        logical :: IsNull = .true.
    end type BNodePointer

    type BNode
        integer :: KeysNum = 0
        integer, dimension(0:10) :: Keys
        integer :: ChildrenNum
        logical :: Leaf
        logical :: Found = .false.
        integer :: FoundIdx
        
        type(BNodePointer), dimension(0:10) :: Children

    end type BNode
contains
    subroutine Init(this)
        type(BNode), intent(inout) :: this

        this%KeysNum = 0
        this%Keys = 0
        this%ChildrenNum = 0
        this%Leaf = .false.
        
        !maybe init Children... or maybe sometime..

    end subroutine Init

end module btreenode 