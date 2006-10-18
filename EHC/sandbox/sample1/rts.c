#include "rts.h"

Pointer SP, RP, BP ;
Pointer Stack, ReturnArea;

#if USE_BOEHM_GC
#else
Pointer HP;
Pointer Heap;
Pointer HeapEndCAF, HeapLimit;
#endif

extern int fun_main();

int main(int argc, char** argv)
{
#if USE_BOEHM_GC
    GC_INIT() ;

    Stack = (Pointer)GC_MALLOC_UNCOLLECTABLE(SIZEOF_GRWORD*STACKSIZE);
    ReturnArea = (Pointer)GC_MALLOC_UNCOLLECTABLE(SIZEOF_GRWORD*RETURNSIZE);
#else
    Heap = (Pointer)malloc(sizeof(GrWord)*HEAPSIZE);

    HeapLimit = Heap + HEAPSIZE;

    HP = Heap;

    Stack = (Pointer)malloc(sizeof(GrWord)*STACKSIZE);
    ReturnArea = (Pointer)malloc(sizeof(GrWord)*RETURNSIZE);
#endif
    
    SP = Stack;
    RP = ReturnArea;

    initialize();
#if USE_BOEHM_GC
#else
    HeapEndCAF = HP;
#endif
    fun_main();


#if USE_BOEHM_GC
     printf("result SP-offset=%d tag=%d value=%d\n", SP-Stack, RP[0], RP[1] );
#else
     printf("result SP-offset=%d HP-offset=%d tag=%d value=%d\n", SP-Stack, HP-Heap, RP[0], RP[1] );
#endif

    return 0;
}

#if USE_BOEHM_GC
#else
GrWord heapalloc(int n)
{
    GrWord res = (GrWord) HP;
    HP += n;
    if (HP>=HeapLimit)
    {
        printf("heap overflow\n");
        exit(1);
    }

    return res;
}
#endif
