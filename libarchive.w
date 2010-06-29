#!chezscheme
(@chezweb)

"\\centerline{
  \\titlef Libarchive: Stream-oriented Archive Reading and Writing}
\\bigskip
\\centerline{Aaron W. Hsu {\\tt <arcfide@sacrideo.us>}}
\\medskip
\\centerline{\\today}\\par
\\bigskip\\rendertoc\\par
\\vfill
\\noindent
Copyright $\\copyright$ 2010 Aaron W. Hsu {\\tt <arcfide@sacrideo.us>}
\\medskip\\noindent
Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.
\\medskip\\noindent
THE SOFTWARE IS PROVIDED ``AS IS'' AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.\\par\\break
"

(@* "Shared Object Files" 
"The following library requires that the libarchive shared object file
is visible and loaded."

(@c
(load-shared-object "libarchive.so")
))

(@l "This library wraps the functionality of the libarchive library.
Libarchive provides a convenient API for reading and writing of
stream-oriented archive formats.

The best source of documentation for the libarchive functions is the man
pages where they are documented. This document does not attempt to
duplicate the information found there.

{\\it Important Note:} This library is currently incomplete and needs
work before it can be considered a complete library."

(arcfide libarchive)
(export make-archive-read archive-read-close archive-read-finish)
(import (chezscheme))

(@* 2 "Generic archive structures"
"There are two main generic archive structures for libarchive, archive
structs and archive entry structs. In the archive record, we have a
special field for objects that we lock in the process of working with
the archive. When the archive is no longer needed, we can unlock these
objects."

(@c
(define-record-type archive (fields (mutable ptr) (mutable locked))
  (protocol
    (lambda (n)
      (lambda (ptr)
        (assert (and (integer? ptr) (exact? ptr) (positive? ptr)))
        (n ptr '())))))
(define-record-type archive-entry (fields (mutable ptr)))
))

(@* "Reading Archives"
"To read a given archive, the general process goes something like this:

\\orderedlist
\\li Open a new archive object.
\\li Set/configure its options
\\li Open the archive
\\li Iterate over the archive entries, 
\\li Possibly read data at the various entries.
\\li Close the archive
\\endorderedlist

\\noindent The Scheme equivalent should work just like this, except that
this should work a little like ports in that an archive should be closed
automatically if the archive object is collected. Otherwise, we can
explicitly finish the object.")

(@* 2 "Opening an archive for reading"
"Opening an archive object is accomplished by the |archive_read_new()|
foreign function. It returns a pointer to an opaque |struct archive|
structure:

\\medskip\\verbatim
struct archive *archive_read_new(void);
|endverbatim
\\medskip

\\noindent We want to wrap this function into a function that has a
signature more like this:

\\medskip\\verbatim
(make-archive-read) => archive-object
|endverbatim
\\medskip

\\noindent The |archive-object| should be its own disjoint record type,
and it should have a single field that contains the pointer value.

We also want to make sure that the allocated pointers are cleaned up
after they are run, so we want to create a collection handler that is run
when appropriate."

(@c
(define archive-read-guardian (make-guardian))
(define-record-type archive-read (parent archive)
  (protocol
    (lambda (p)
      (let ([new (foreign-procedure "archive_read_new" () uptr)])
        (lambda ()
          (let ([ptr (new)])
            (if (zero? ptr)
                (error 'make-archive-read 
                  "Unable to make foreign archive structure.")
                (let ([res ((p ptr))])
                  (archive-read-guardian res)
                  res))))))))
(define (archive-read-collect)
  (let loop ([arc (archive-guardian)])
    (when arc 
      (archive-read-finish arc) 
      (loop (archive-guardian)))))
))

(@* 2 "Options for compression and format support"
"Normally we select what the formats and compression algorithms are that
we want to support for this archive stream. There are a number of these
different formats.

\\medskip
{\\noindent\\bf Compression support:}

\\unorderedlist
\\li bzip2
\\li compress
\\li gzip
\\li lzma
\\li none
\\li xz
\\li program
\\li program with signature
\\endunorderedlist

{\\noindent\\bf Format Support:}

\\unorderedlist
\\li ar
\\li cpio
\\li empty
\\li iso9660
\\li mtree
\\li raw
\\li tar
\\li zip
\\endunorderedlist

\\noindent Other than the program compression formats, the foreign
procedures to all of these have a similiar signature:

\\medskip\\verbatim
int archive_read_support_???_???(struct archive *);
|endverbatim
\\medskip

\\noindent It makes sense to define all of these functions with a common
syntax then, which looks like this:

\\medskip\\verbatim
(define-archive-read-support-proc name foreign-name)
|endverbatim
\\medskip

\\noindent The above syntax will define |name| as below:

\\medskip\\verbatim
(define name
  (let ([ff (foreign-procedure foreign-name (uptr) int)])
    (lambda (arc)
      (assert (archive-read? arc))
      (let ([ptr (archive-ptr arc)])
        (unless ptr (error 'name \"Finished archive encountered\" arc))
        (let ([res (ff ptr)])
          (unless (zero? res)
            (archive-error 'name arc)))))))
|endverbatim
\\medskip

\\noindent These procedures should then match a simple signature that
takes a single |archive-read| object and which have an unspecified
return value."

(@c
(define-syntax define-archive-read-support-proc
  (syntax-rules ()
    [(_ name foreign-name)
     (define name
       (let ([ff (foreign-procedure foreign-name (uptr) int)])
         (lambda (arc)
           (assert (archive-read? arc))
           (let ([ptr (archive-ptr arc)])
             (unless ptr (error 'name "archive is already finished" arc))
             (let ([res (ff ptr)])
               (unless (zero? res)
                 (archive-error 'name arc)))))))]))
))

(@ "And now we'll define the support setting format support."
  
(@c
(define-archive-read-support-proc 
  archive-read-support-format-all "archive_read_support_format_all")
(define-archive-read-support-proc 
  archive-read-support-format-ar "archive_read_support_format_ar")
(define-archive-read-support-proc 
  archive-read-support-format-cpio "archive_read_support_format_cpio")
(define-archive-read-support-proc 
  archive-read-support-format-empty "archive_read_support_format_empty")
(define-archive-read-support-proc 
  archive-read-support-format-iso9660 "archive_read_support_format_iso9660")
(define-archive-read-support-proc 
  archive-read-support-format-mtree "archive_read_support_format_mtree")
(define-archive-read-support-proc 
  archive-read-support-format-raw "archive_read_support_format_raw")
(define-archive-read-support-proc 
  archive-read-support-format-tar "archive_read_support_format_tar")
(define-archive-read-support-proc 
  archive-read-support-format-zip "archive_read_support_format_zip")
))

(@ "And now we'll define the support setting compression support."

(@c
(define-archive-read-support-proc 
  archive-read-support-compression-all "archive_read_support_compression_all")
(define-archive-read-support-proc 
  archive-read-support-compression-bzip2 "archive_read_support_compression_bzip2")
(define-archive-read-support-proc 
  archive-read-support-compression-compress "archive_read_support_compression_compress")
(define-archive-read-support-proc 
  archive-read-support-compression-gzip "archive_read_support_compression_gzip")
(define-archive-read-support-proc 
  archive-read-support-compression-none "archive_read_support_compression_none")
(define-archive-read-support-proc 
  archive-read-support-compression-lzma "archive_read_support_compression_lzma")
(define-archive-read-support-proc 
  archive-read-support-compression-xz "archive_read_support_compression_xz")
))

(@* 2 "Setting Options"
"There are three procedures for configuring and setting objects:

\\medskip\\verbatim
int archive_read_set_filter_options(struct archive *, const char *);
int archive_read_set_format_options(structu archive *, const char *);
int archive_read_set_options(struct archive *, const char *);
|endverbatim
\\medskip

\\noindent These let you set the various options. The documentation for
how the strings should look can be found in the libarchive man pages.
Otherwise, these are simple wrappers."

(@c
(define-syntax define-archive-read-option-setter
  (syntax-rules ()
    [(_ name foreign-name)
     (define name
       (let ([set! (foreign-procedure foreign-name (uptr string) int)])
         (lambda (a s)
           (assert (archive-read? a))
           (assert (string? s))
           (unless (archive-ptr a)
             (error 'name "archive already finished" a))
           (let ([res (set! (archive-ptr a) s)])
             (unless (zero? res)
               (archive-error 'name a))))))]))
(define-archive-read-option-setter
  archive-read-filter-options-set! "archive_read_set_filter_options")
(define-archive-read-option-settter
  archive-read-format-options-set! "archive_read_set_format_options")
(define-archive-read-options-setter
  archive-read-options-set! "archive_read_set_options")
))

(@* 2 "Opening read archives"
"In the libarchive paradigm, there are generalized open procedures that
take callbacks for returning the data from various sources, and
convenience procedures that allow you to open common C data types, such
as FIlE pointers, file descriptors, memory regions, and paths. Since
Scheme does not have these same paradigms, we will define more
Scheme-like paradigms below.

The easiest one is the filename, which is basically the same as
|archive_read_open_filename()|. 

\\medskip\\verbatim
(archive-read-open-filename archive path block-size)
|endverbatim
\\medskip

\\noindent This will open the archive on the given file with the given
block size."

(@c
(define archive-read-open-filename
  (let ([open (foreign-procedure "archive_read_open_filename" 
                (uptr string unsigned-long) int)])
    (lambda (a p bs)
      (assert (archive-read? a))
      (assert (string? p))
      (assert (and (integer? bs) (exact? bs) (not (negative? bs))))
      (let ([ptr (archive-ptr a)])
        (if ptr
            (let ([res (open ptr p bs)])
              (unless (zero? res)
                (archive-error 'archive-read-open-filename a)))
            (error 'archive-read-open-file
              "archive already finished" a))))))
))

(@ "It is also possible to open archives based on memory regions. This
corresponds in Scheme to dealing with bytevectors, so we wrap this
around as appropriate. The idea here is to make it easy to recur on the
various data elements if that is important.

\\medskip\\verbatim
(archive-read-open-bytevector archive bytevector)
|endverbatim
\\medskip

\\noindent This will open the archive specified using the bytevector
given. Because we need to have a constant and reliable space, we have to
remember to lock the object and add it to the archives set of
associated locked objects."

(@c
(define archive-read-open-bytevector
  (let ([open (foreign-procedure "archive_read_open_memory" 
                (uptr u8* long) int)])
    (lambda (a buf)
      (assert (archive-read? a))
      (assert (bytevector? buf))
      (let ([ptr (archive-ptr a)])
        (unless ptr 
          (error 'archive-read-open-bytevector 
            "archive already finished" a))
        (lock-object buf)
        (archive-locked-set! a (cons buf (archive-locked a)))
        (let ([res (open ptr buf (bytevector-length buf))])
          (unless (zero? res)
            (archive-error 'archive-read-open-bytevector a)))))))
))

(@ "XXX TODO: Write a binding for the |archive_read_open()| and
|archive_read_open2()| procedures.")

(@* 2 "Iterating over headers"
"Generally, when dealing with a stream, you will iterate over the
headers, inspecting the information until you find one that has data
that you want. You can then use the data procedures documented below to
extract this information out. Otherwise, if you don't care about the
entry, you will move on to the next entry. The libarchive API does this
with the following procedures:

\\medskip\\verbatim
int archive_read_next_header(struct archive *, struct archive_entry **);
int archive_read_next_header2(struct archive *, struct archive_entry *);
|endverbatim
\\medskip

\\noindent The first version just makes available an entry from an
already existing internal entry. The second actually populates an entry
that you give it. We don't have much need for the first, since we aren't
trying to avoid allocation here. It would be nice to have an interface
like this:

\\medskip\\verbatim
(archive-read-next-header archive) => archive-entry
|endverbatim
\\medskip

\\noindent Obviously we don't want these headers to suddenly change.
This makes the only reasonable interface one where we control and
allocate a new archive entry every time we call the function. For this
reason we will ignore the first function."

(@c
(define archive-read-next-header 
  (let ([next (foreign-procedure "archive_read_next_header2"
                (uptr uptr) int)])
    (lambda (arc)
      (assert (archive-read? arc))
      (let ([ptr (archive-ptr arc)])
        (unless ptr 
          (error 'archive-read-next-header 
            "archive already finished" arc))
        (let* ([entry (make-archive-entry)]
               [entry-ptr (archive-entry-ptr entry)])
          (let ([res (next ptr entry-ptr)])
            (if (zero? res)
                entry
                (archive-error 'archive-read-next-header arc))))))))
))

(@* 2 "Reading data out of archive entries"
"Even though we may have information inside the headers about a given
entry in our archive, this doesn't contain the actual data of the entry.
If we want to do this, we need to actually call some read functions that
the API defines. The libarchive provides the following non-deprecated
procedures for this task:

\\medskip\\verbatim
ssize_t archive_read_data(struct archive *, void *buff, size_t len);
int 
archive_read_data_block(
    struct archive *, const void **buff, size_t *len, off_t *offset);
int archive_read_data_skip(struct archive *);
int archive_read_data_into_fd(struct archive *, int fd);
|endverbatim
\\medskip

\\noindent The |archive_read_data()| procedure only seems useful to me
if you want to specify the buffer size yourself, but we're sort of doing
this with our own block size anyways. Moreover, you can't handle sparse
files correctly with this procedure, so I'd prefer to just avoid using
it entirely, and instead focus on using the more general
|archive_read_data_block()| procedure, which actually avoids copying
data (we'll have to do some of that anyways) and can correctly handle
sparse files. The skip procedure allows us to skip all of the data in a
given entry. I haven't figured out why it is important, actually.

The |archive_read_data_into_fd()| procedure is basically a way to write
and entire archive entry into a file descriptor, which corresponds
nicely to the Scheme idea of writing to an output port. We'll want to
make sure that this works mostly the same way, but we'll need to
implement it natively in Scheme because we have no way to convert output
ports into file descriptors.

Anyways, on to the main and primarily useful function.

\\medskip\\verbatim
(archive-read-data-block archive) => bytevector offset
|endverbatim
\\medskip

\\noindent The |archive-read-data-block| procedure gives us back a
bytevector containing the data of the entry starting at the given
offset. Repeated calls to this procedure will give us the next blocks
each time. The same guarantees as |archive_read_data_block()| apply, in
that each block will not overlap with the last, and that each offset
will increase.

XXX TODO: I need some information about when and how I can tell when
I've read the last block."

(@> |Define archive-read-data-block| (export archive-read-data-block)
(define archive-read-data-block
  (let ([read-block (foreign-procedure "archive_read_data_block"
                      (uptr uptr uptr uptr) int)])
    (lambda (arc)
      (assert (archive-read? arc))
      (let ([ptr (archive-ptr arc)])
        (unless ptr
          (error 'archive-read-data-block
            "archive already finished" arc))
        (let ([uptr/size (foreign-sizeof 'uptr)])
          (let ([buf* (foreign-alloc uptr/size)]
                [len* (foreign-alloc uptr/size)]
                [off* (foreign-alloc uptr/size)])
            (let ([res (read-block ptr buf* len* off*)])
              (if (zero? res)
                  (@< |Return buffer and offset| buf* len* off*)
                  (@< |Free foreign blocks and raise error| 
                      arc buf* len* off*)))))))))
))

(@ "If we have a successful call to the |archive_read_data_block()|
procedure, then we should now have a pointer to a buffer, a length of
that buffer, and the offset of that buffer relative to the current
entry. We want to return a bytevector and an offset value instead of
these pointers, so we need to extract out and copy the values into the
bytevector and convert the offset."

(@> |Return buffer and offset| (capture buf* len* off*)
(let ([len (foreign-ref 'long len* 0)]
      [offset (foreign-ref 'long off* 0)]
      [fbuf (foreign-ref 'uptr buf* 0)])
  (let ([buf (make-bytevector len)])
    (do ([i 0 (+ i 1)])
        [(>= i len)]
      (bytevector-u8-set! buf i (foreign-ref 'unsigned-8 buf i)))
    (foreign-free buf*)
    (foreign-free len*)
    (foreign-free off*)
    (values buf offset)))
))

(@ "Unlike the normal error situations, we also have our three foreign
allocated blocks of memory that we should free whenever we encounter
such an error case to avoid creating leaks."

(@> |Free foreign blocks and raise error| (capture arc buf* len* off*)
(foreign-free buf*)
(foreign-free len*)
(foreign-free off*)
(archive-error 'archive-read-data-block arc)
))

(@ "We'll put the |archive-read-data-block| procedure into the library
top-level."

(@c
(@< |Define archive-read-data-block|)
))

(@ "We also want to be able to do the simple skipping over the archive
entry data, which is provided by the following procedure:

\\medskip\\verbatim
(archive-read-data-skip archive)
|endverbatim
\\medskip

\\noindent This behaves just like the |archive_read_data_skip()|
procedure."

(@c
(define archive-read-data-skip
  (let ([skip (foreign-procedure "archive_read_data_skip" (uptr) int)])
    (lambda (arc)
      (assert (archive-read? arc))
      (let ([ptr (archive-ptr arc)])
        (unless ptr
          (error 'archive-read-data-skip
            "archive already finished" arc))
        (let ([res (skip ptr)])
          (unless (zero? res)
            (archive-error 'archive-read-data-skip arc)))))))
))

(@ "It's also nice to be able to send everything to an output port.
We'd like something like this:

\\medskip\\verbatim
(archive-read-data-port archive binary-output-port)
|endverbatim
\\medskip

\\noindent The incoming output port must be a binary port and we will
make use of the |set-port-position!| procedure to move along offsets, so
we need to make sure that the given port supports these operations.

XXX TODO: Is this really how you use |archive-read-data-block|? I'm
unsure of how to tell when we're at the end of the block."

(@c
(define (archive-read-data-port arc op)
  (assert (archive-read? arc))
  (assert (and (output-port? op) (binary-port? op)))
  (assert (port-hash-set-port-position!? op))
  (let ([ptr (archive-ptr arc)])
    (unless ptr
      (error 'archive-read-data-port 
        "archive already finished" arc))
    (let loop ()
      (let-values ([(buf off) (archive-read-data-block arc)])
        (unless (zero? (bytevector-length buf))
          (set-port-position! op off)
          (put-bytevector op buf)
          (loop))))))
))
          

(@* 2 "Extracting from read archives"
"XXX TODO: I'm not sure I actually understand what the purpose of these
procedures are, so I'm going to stay away from them right now and not
implement them. We need to get back to this sooner or later, though.")

(@* 2 "Closing read archives"
"Closing an archive completes the archive and invokes its close
callback. The |archive-read-close| procedure takes a single argument,
that should be an |archive-read| object.

\\medskip\\verbatim
(archive-read-close archive-read-object)
|endverbatim
\\medskip

\\noindent At the moment we don't do very intelligent error handling, and
we just signal a fatal error the moment that we encounter any
non-success exit status."

(@c
(define archive-read-close 
  (let ([close (foreign-procedure "archive_read_close" (uptr) int)])
    (lambda (arc)
      (assert (archive-read? arc))
      (let ([ptr (archive-ptr arc)])
        (when ptr
          (let ([res (close ptr)])
            (unless (zero? res)
              (archive-error 'archive-read-close arc))))))))
))

(@* 2 "Finishing read archives"
"Finishing an archive closes the archive if it has not already been
closed, and then frees up and releases all of its resources. The
|archive-read-finish| procedure should be passed a single |archive-read|
object.

\\medskip\\verbatim
(archive-read-finish archive-read-object)
|endverbatim
\\medskip

\\noindent At the moment we don't do very intelligent error handling, and
we just signal a fatal error the moment that we encounter any
non-success exit status.

By the time that we have successfully finished an archive, there will be
no further need for it, and we should make sure to signal to our process
that we have already finished the archive. This prevents the collection
requests from trying to collect or finish the archives a second time. To
do this we simply |set!| the pointer to false.

Since we may be associating and storing a number of structures that were
created in Scheme in the foreign side of the system, we lock these
objects to ensure that they aren't collected. When we finish an archive,
we must remember to unlock these objects so that they do not adversely
affect the storage manager."

(@c
(define archive-read-finish
  (let ([finish (foreign-procedure "archive_read_finish" (uptr) int)])
    (lambda (arc)
      (assert (archive-read? arc))
      (let ([ptr (archive-ptr arc)])
        (when ptr
          (map unlock-object (archive-locked arc))
          (archive-locked-set! arc '())
          (let ([res (finish ptr)])
            (if (zero? res)
                (archive-ptr-set! arc #f)
                (archive-error 'archive-read-finish arc))))))))
))

(@* "Working with archive entries"
"XXX TODO")

(@* "Archive Errors"
"XXX TODO")

(@* "Registering the collection handlers"
"If the client wants to do the collection of the archives automatically,
the collection procedures must be registered as part of the collection
handlers. The following procedure is provided to make this easier."

(@c
(define (register-archive-collection-handlers)
  (let ([current-handler (collect-request-handler)])
    (collect-request-handler
      (lambda ()
        (archive-read-collect)
        (current-handler)))))
))
)
