#lang racket/base

(require
 "interpreter.rkt"
 racket/promise)

(provide
 load-device-file  ;; populate DB
 part              ;; to parameterize queries

 ;; (These can be kept private: the exported identifiers below are
 ;; sufficient.  See 'define-reader/provide)
 
 ;; script         ;; scripts indexed by number
 ;; property       ;; use exported accessors instead (see 'define-reader/provide)
 )


(define part (make-parameter 'PIC18F1220))


;; PICkit2 config file parser.  This dumps as much as possible in the
;; local namespace, to reduce symbolic lookups.

;; Abbrev
(define band bitwise-and)
(define bxor bitwise-xor)
(define <<<  arithmetic-shift)
(define (>>> n b) (<<< n (- b)))

;; Scalars
(define (little-endian . bytes)
  (if (null? bytes) 0
      (+ (car bytes)
         (* 256 (apply little-endian (cdr bytes))))))
(define (signed value bits)
  (let ((sign (band (<<< 1 (sub1 bits)))))
    (- (bxor value sign) sign)))
(define b        read-byte)
(define uchar    b)
(define (uint64) (little-endian (b) (b) (b) (b) (b) (b) (b) (b)))
(define (uint)   (little-endian (b) (b) (b) (b)))
(define (ushort) (little-endian (b) (b)))
(define (int)    (signed (uint) 32))
(define (float)  (32bits->float (uint)))
(define bool     uchar)
(define (sid)    (script-ref (ushort)))

;; i can't find a library function that does this.. srfi-56 would help..
(define (32bits->float num)
  (if (zero? num) 0.0
      (let ((sign      (band 1    (>>> num 31)))
            (exponent  (band #xFF (>>> num 23)))
            (base      (band #x7FFFFF num)))
        (* (- 1 (* 2 sign))
           (expt 2 (- exponent 127))
           (+ 1.0 (/ base #x800000.))))))
               

;; Arrays
(define (array type [size #f])
  (if (not size)
      (type)
      (for/list ((i (in-range size))) (type))))
(define (ushortstring)
  (array ushort (ushort)))

;; Strings
(define (string)
  (read-bytes
   (let ((n (b)))
     (if (= 0 (band n #x80))
         n
         (+ (band n #x7f)
            (* #x80 (b)))))))

;; Read whole file.
(define *file* #f)
(define *family* #f)
(define *part* #f)
(define *script* #f)
(define *part-index* #f)

(define (load-device-file file)
  (define (_table id reader)
    (list->vector
     (for/list ((i (in-range (hash-ref *file* id)))) (reader))))
  (define-syntax-rule (table var id reader)
    (set! var (_table 'id reader)))
  (with-input-from-file file
    (lambda ()
      (set!  *file*                  (DeviceFileParams))
      (table *family*  NumberFamilies DeviceFamilyParams)
      (table *part*    NumberParts    DevicePartParams)
      (table *script*  NumberScripts  DeviceScripts)))
  (set! *part-index* (make-hash))
  (for ((p *part*))
     (hash-set! *part-index*
                (string->symbol
                 (bytes->string/utf-8 
                  (hash-ref p 'PartName)))
                p)))

;; QUERIES

(define script-debug (make-parameter #t))

(define (script n)
  (and (not (zero? n))
       (vector-ref *script* (sub1 n))))
(define (script-ref n)
  (lambda ([postproc
            (lambda (h)  ;; default drops tags + wraps in struct
              (when (script-debug)
                (dasm h))
              (make-scr
               (for/list ((c (hash-ref h 'Script)))
                  (band c #xFF))))])
    (let ((s (script n)))
      (and s (postproc s)))))

;; Delegate: if the part doesn't have a certain tag, propagate the
;; request to the family.  All the part and familily queries are
;; exported as thunks, parameterized by 'part

(define (property tag [dev (part)])
  (let* ((part (hash-ref *part-index* dev))
         (fam  (vector-ref *family* (hash-ref part 'Family))))
    (hash-ref part tag
              (lambda ()
                (hash-ref fam tag)))))
  

;; Create reader from table.
(define-syntax-rule (define-reader name (type id . args) ...)
  (define (name)
    (make-immutable-hash
     (list (cons 'id (apply array type 'args)) ...))))

;; Also create accessor thunks for properties on top of hash DB.
;; These will get the property of the current part (makes
;; name-checking static).
(define (do-property id . args)
  (let ((p (property id)))
    (if (procedure? p) (apply p args) p)))

(define-syntax-rule (define-reader/provide name (type id . args) ...)
  (begin
    (define-reader name (type id . args) ...)
    (begin (define (id . a) (apply do-property 'id a)) ...)
    (provide id ...)))
         

;; Write pretty-printed
(define (dat->tree dat)
  (hash-map
   dat
   (lambda (key value)
     (cons key
           (if (hash? value)
               (hash-map value cons)
               (map
                (lambda (h)
                  (hash-map h cons))
                value))))))


(define-reader DeviceFileParams
 (int VersionMajor)
 (int VersionMinor)
 (int VersionDot)
 (string VersionNotes) ;;[512]
 (int NumberFamilies)
 (int NumberParts)
 (int NumberScripts)
 (uchar Compatibility)
 (uchar UNUSED1A)
 (ushort UNUSED1B)
 (uint UNUSED2))

(define-reader/provide DeviceFamilyParams
 (ushort FamilyID)
 (ushort FamilyType)
 (ushort SearchPriority)
 (string FamilyName) ;; [24]
 (sid ProgEntryScript)
 (sid ProgExitScript)
 (sid ReadDevIDScript)
 (uint DeviceIDMask)
 (uint BlankValue)
 (uchar BytesPerLocation)
 (uchar AddressIncrement)
 (bool PartDetect)
 (sid ProgEntryVPPScript)
 (ushort UNUSED1)
 (uchar EEMemBytesPerWord)
 (uchar EEMemAddressIncrement)
 (uchar UserIDHexBytes)
 (uchar UserIDBytes)
 (uchar ProgMemHexBytes)
 (uchar EEMemHexBytes)
 (uchar ProgMemShift)
 (uint TestMemoryStart)
 (ushort TestMemoryLength)
 (float Vpp))

(define-reader/provide DevicePartParams
 (string PartName) ;; [28]
 (ushort Family)
 (uint DeviceID)
 (uint ProgramMem)
 (ushort EEMem)
 (uint EEAddr)
 (uchar ConfigWords)
 (uint ConfigAddr)
 (uchar UserIDWords)
 (uint UserIDAddr)
 (uint BandGapMask)
 (ushort ConfigMasks 8)
 (ushort ConfigBlank 8)
 (ushort CPMask)
 (uchar CPConfig)
 (bool OSCCALSave)
 (uint IgnoreAddress)
 (float VddMin)
 (float VddMax)
 (float VddErase)
 (uchar CalibrationWords)
 (sid ChipEraseScript)
 (sid ProgMemAddrSetScript)
 (uchar ProgMemAddrBytes)
 (sid ProgMemRdScript)
 (ushort ProgMemRdWords)
 (sid EERdPrepScript)
 (sid EERdScript)
 (ushort EERdLocations)
 (sid UserIDRdPrepScript)
 (sid UserIDRdScript)
 (sid ConfigRdPrepScript)
 (sid ConfigRdScript)
 (sid ProgMemWrPrepScript)
 (sid ProgMemWrScript)
 (ushort ProgMemWrWords)
 (uchar ProgMemPanelBufs)
 (uint ProgMemPanelOffset)
 (sid EEWrPrepScript)
 (sid EEWrScript)
 (ushort EEWrLocations)
 (sid UserIDWrPrepScript)
 (sid UserIDWrScript)
 (sid ConfigWrPrepScript)
 (sid ConfigWrScript)
 (sid OSCCALRdScript)
 (sid OSCCALWrScript)
 (ushort DPMask)
 (bool WriteCfgOnErase)
 (bool BlankCheckSkipUsrIDs)
 (ushort IgnoreBytes)
 (sid ChipErasePrepScript)
 (uint BootFlash)
 (uint UNUSED4)
 (sid ProgMemEraseScript)
 (sid EEMemEraseScript)
 (sid ConfigMemEraseScript)
 (sid reserved1EraseScript)
 (sid reserved2EraseScript)
 (sid TestMemoryRdScript)
 (ushort TestMemoryRdWords)
 (sid EERowEraseScript)
 (ushort EERowEraseWords)
 (bool ExportToMPLAB)
 (sid DebugHaltScript)
 (sid DebugRunScript)
 (sid DebugStatusScript)
 (sid DebugReadExecVerScript)
 (sid DebugSingleStepScript)
 (sid DebugBulkWrDataScript)
 (sid DebugBulkRdDataScript)
 (sid DebugWriteVectorScript)
 (sid DebugReadVectorScript)
 (sid DebugRowEraseScript)
 (ushort DebugRowEraseSize)
 (sid DebugReserved5Script)
 (sid DebugReserved6Script)
 (sid DebugReserved7Script)
 (sid DebugReserved8Script)
 (sid DebugReserved9Script))

(define-reader DeviceScripts
 (ushort ScriptNumber)
 (string ScriptName) ;; [32]
 (ushort ScriptVersion)
 (uint UNUSED1)
 ;;(ushort ScriptLength)
 ;;(ushort Script[64])
 (ushortstring Script)
 (string Comment)) ;; [128]



;; Test
;(define dat  (read-device-file "/tmp/test.dat"))
  

