/*
NAME: Alex Yu
EMAIL: alexy23@g.ucla.edu
ID: 105295708
*/

#include <sched.h>
#include <stdlib.h>
#include "SortedList.h"

void SortedList_insert(SortedList_t *list, SortedListElement_t *element) {
    SortedListElement_t *curr = list;
    while(curr->next != list && *(curr->next->key) < *(element->key)) {
        curr = curr->next;
    }

    if(opt_yield & INSERT_YIELD) {
        sched_yield();
    }

    SortedListElement_t *next = curr->next;
    curr->next = element;
    element->prev = curr;
    element->next = next;
    next->prev = element;
}

int SortedList_delete(SortedListElement_t *element) {
    SortedListElement_t *prev = element->prev, *next = element->next;
    if(prev == NULL || next == NULL) return 1;
    if(prev->next != element) return 1;
    if(next->prev != element) return 1;

    if(opt_yield & DELETE_YIELD) {
        sched_yield();
    }

    prev->next = next;
    next->prev = prev;

    return 0;
}

SortedListElement_t *SortedList_lookup(SortedList_t *list, const char *key) {
    SortedListElement_t *curr = list->next;   
    while(curr != list && curr->key != NULL && curr->key != key) {
        curr = curr->next;

        if(opt_yield & LOOKUP_YIELD) {
            sched_yield();
        }
    }

    if(curr->key == NULL || curr == list) return NULL;
    return curr;
}

int SortedList_length(SortedList_t *list) {
    int length = 0;
    SortedListElement_t *curr = list;

    while(curr->next != list) {
        SortedListElement_t *prev = curr->prev, *next = curr->next;
        if(prev == NULL || (prev != NULL && prev->next != curr)) return -1;
        if(next == NULL || (next != NULL && next->prev != curr)) return -1;

        curr = curr->next;
        length++;

        if(opt_yield & LOOKUP_YIELD) {
            sched_yield();
        }
    }

    return length;
}
