(in-package :lc3)

(comment "Memory routines" :big t)
;; We have a virtual memory consisting of 16 pages of 256 words each.

(procedure (read-word position)
    ((stashed-position 0)
     (mask #xff))
  ;; Compute page number.
  (st position 'stashed-position)
  (mov r2 position)
  (mov r1 8)
  (mov r0 8)
  (jsr 'ldb)
  ;; Load from page table.
  (lea r1 'page-table)
  (add r0 r0 r1)
  (ldr r0 r0 0)
  ;; Is page allocated? If so, return zero, else read page.
  (polarity-case
    (:zero
     ;; r0 is already zero.
     (return))
    ((:negative :positive)
     ;; Compute offset into table.
     (ld r1 'stashed-position)
     (ld r2 'mask)
     (and r1 r1 r2)
     (add r0 r0 r1)
     ;; Finally load from physical memory. 
     (ldr r0 r0 0)
     (return))))

(procedure (allocate-page)
    ((page-size 256))
  (ld r0 'next-page)
  (ld r1 'page-size)
  (add r1 r1 r0)
  (st r1 'next-page)
  (return))

(procedure (write-word position value)
    ((stashed-position 0)
     (stashed-value 0)
     (mask #xff))
  ;; Compute page number.
  (st position 'stashed-position)
  (st value 'stashed-value)
  (mov r2 position)
  (mov r1 8)
  (mov r0 8)
  (jsr 'ldb)
  ;; Load from page table.
  (lea r1 'page-table)
  (add r0 r0 r1)
  (ldr r0 r0 0)
  (polarity-case
    (:zero
     ;; Grab a page.
     (jsr 'allocate-page)))
  ;; Now that we have a page, write to it.
  (ld r1 'stashed-position)
  (ld r2 'mask)
  (and r1 r1 r2)
  (add r0 r0 r1)
  (ld r3 'stashed-value)
  (str r3 r0 0)
  (return))
