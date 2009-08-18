;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Descot Web Application About Page
;;; 
;;; Copyright (c) 2009 Aaron Hsu <arcfide@sacrideo.us>
;;; 
;;; Permission to use, copy, modify, and distribute this software for
;;; any purpose with or without fee is hereby granted, provided that the
;;; above copyright notice and this permission notice appear in all
;;; copies.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;;; PERFORMANCE OF THIS SOFTWARE.

(module ()
  (import scheme)
  (import descot-web-generators)
  (import descot-web-parameters)
  (import descot-web-utilities)

(define about-page
  (lambda ()
    (descot-wrapper "About" descot-stylesheet
      `(div (@ (class "left_box"))
	 (a (@ (href "/10-rdf-schema")) "Descot RDF Schema")
	 " | "
	 (a (@ (href "http://www.w3.org/TR/rdf-primer/")) "RDF Primer")
	 " | " 
	 (a (@ (href "/doc/descot_techdoc.pdf")) "Technical Documentation"))
      (h3 "Overview")
      (p
        "Descot is a project to design a Scheme Source/Library
         repository centered around the idea of developer liberty
         and the principle of least invasion/burden.  A Scheme
         repository ought not to cause or impose needless restrictions
         on a developer who wishes to utilize the repository.
         Instead, the repository should facilitate more rapid and
         convenient development for that developer, rather than
         hamper it by enforcing artificial changes to that developer's
         workflow.")
      (h3 "Goals")
      (ol
	(li "To aggregate and bring together as much Scheme code as possible.")
	(li "To deliver portable/easy-to-port code.")
	(li "To deliver high quality code.")
	(li "To reduce developer burden when attempting to use libraries."))
      (h3 "Plans")
      (p
	"Current plans are to design a working and useful vocabulary for 
	 describing Scheme libraries, develop an infrastructure for the 
	 decentralized distribution of this meta information, and provide 
	 a search engine for collecting and querying these repositories.")
      (p
	"This will make it easier for new and experienced users to 
	 both distribute and find code, and hopefully allow the Scheme 
	 community to share the common benefits of a repository of common 
	 code, without the troubles of ugly frameworks of implementation 
	 specific tools.")
      (p
	"Once the current goals of documenting the Scheme Library 
	 situation is complete, progress can be made towards streamlining 
	 the deployment of Descot application, and futher consideration 
	 can be made towards semi-automated porting of libraries from 
	 one system to another.")
      (h3 "Benefits and Reasons to use Descot")
      (p "Library repositories are judged by three main criteria:")
      (ul
	(li "How up-to-date and how many libraries are available")
	(li "How easy it is to use")
	(li "How well the system integrates with existing workflows"))
      (p 
	"Current library repositories don't satsify these requirements 
	 sufficiently for the Scheme community. Descot provides the 
	 glue to form a collaborative web of shared code and libraries, 
	 where many servers serving library information can link 
	 together in an effective way, and where users do not have 
	 to use a single toolset to retrieve or manage their libraries.")
      (p 
	"Instead of forcing everyone to host their code on one server, 
	 or maintain two copies of code because they must release to 
	 the general public as well as the particular repositories they 
	 wish to host their libraries, Descot permits authors of 
	 libraries to \"publish once\" and let repositories pick up 
	 their code automatically. It saves the author time by 
	 allowing him to either host his own repository to be mirrored 
	 by others, or to submit just the metadata to a single 
	 repository, and let other repositories for each system 
	 pick it up. Since only the metadata is sent to the repository, 
	 the code is still retrieved from the same location as 
	 everyone else.")
      (p
	"As an user, Descot benefits you because you are no longer 
	 restricted to a specific toolchain for managing your libraries. 
	 Since Descot's interfaces are open and accessible, you can 
	 write your own tools to manage your libraries in the way 
	 you want, or chances are that someone will have already 
	 written them for you. You can access all Descot-compatible 
	 servers without ever having to change the way you want to 
	 work. Descot will integrate better with your Scheme 
	 implementation (using its own package management system 
	 if you so desire), and will let you manage libraries the 
	 way you want.")
      (p
	"Since anyone can easily host their own repositories, 
	 it is easy to get started using Descot, and there is a 
	 much greater potential for the entire Scheme community to 
	 work with Descot, since it doesn't centralize everything into 
	 one way of doing things. This means that the number of 
	 available libraries that you can search easily and 
	 quickly, as well as install without hassle, is much 
	 greater than with a central repository that forces a 
	 specific set of tools on you.")
      (h3 "What Descot is NOT")
      (p
	"Descot is "
	'(strong "not")
	" the entire package. It doesn't solve all the problems 
	 or provide all the tools that you need to manage packages. 
	 It makes it clear how tools are to communicate, and 
	 provides a standard interface for you to work with 
	 repositories, but it doesn't try to say how you should 
	 use the library information available in the repositories. 
	 We intend to provide some basic tools for users and
	 authors, which should work well, but remember that 
	 Descot's focus isn't on the tools, its on how the 
	 tools talk. I hope that the Scheme community will 
	 step up and write their own tools to connect in with 
	 Descot, thus enhancing what people can do with Descot 
	 and making it better for everyone.")
      (p
	"Descot also isn't a specific server for hosting 
	 libraries. It specifies how a server should behave, but 
	 it doesn't force you to use any one server over another. 
	 I provide a server for general use, and I hope to mirror 
	 most of the servers that will be launched, but there 
	 isn't any reason to host library information directly 
	 on my server unless you want to. It is easy to 
	 run your own descot server, and I'm writing more tools 
	 to make it easier everyday.")
      (p 
	"Most importantly, Descot isn't complete yet. It has a 
         lot of cool things in it, but this isn't it! We'll be 
	 adding much more in the days ahead."))))

(call-with-output-file (string-append descot-web-docroot "/about.xhtml")
  (lambda (port)
    (write-html-page about-page port))
  'replace)

)
