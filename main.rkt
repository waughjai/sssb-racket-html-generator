#lang racket

(require racket/format)

(define
    root-template
    '(
        ("h1" "~title")
        ("p" "~desc")
        ("div"
            (
                ("h2" "Categories:" ((style . "font-size:1.75em;margin-top:1em")))
                ("div" "~catlinks")))
        ("div" "~catcontent")))

(define
    category-template
    '(
        ("section"
            (
                ("h2" "~title" ((style . "font-size:1.5em;font-weight:bold;text-transform:uppercase;border-bottom:1px solid #000")))
                ("div" "~content"))
            ((style . "padding-top:6em;margin-top:-3em") (id . "~id")))))

(define
    article-template
    '(
        ("article"
            (
                ("h1" ("a" "~title" ((href . "~url") (_newline . #f))) ((style . "font-size:1.75em;margin-bottom:0.5em")))
                ("div"
                    (("p" "~desc" ((style . "line-height:1.5em"))) ("p" ("a" "Read Article »" ((href . "~url") (_newline . #f)))))))
            ((style . "padding-bottom:0.5em")))))

(define
    (echo text)
    (let
        ((out (open-output-file "output.html" #:exists 'replace)))
        (display text out)
        (close-output-port out)))

(define
    (reduce f list [init '()])
    (foldl f init (reverse list)))

(define
    (replace-map str m)
    (reduce
        (λ
            (a b)
            (if
                (string? (cdr a))
                (string-replace b (~a "~" (symbol->string (car a))) (cdr a))
                b))
        m
        str))

(define indent "\t")

(define
    (generate-indents indent-level)
    (if
        (= indent-level 0)
        ""
        (~a indent (generate-indents (- indent-level 1)))))

(define
    (generate-newline-indent indent-level)
    (~a "\n" (generate-indents indent-level)))

(define
    (add-indents text indent-level)
    (~a (generate-indents indent-level) text))

(define template-newline-default #t)

(define
    (do-if-atts f entry [backup '()])
    (if
        (>= (length entry) 3)
        (f (third entry))
        backup))

(define
    (template-newline? entry)
    (do-if-atts
        (λ
            (atts)
            (if
                (dict-has-key? atts '_newline)
                (dict-ref atts '_newline)
                template-newline-default))
        entry
        template-newline-default))

(define
    (conditional-indent template level)
    (if (template-newline? template) (generate-newline-indent level) ""))

(define
    (generate-attribute-html entry)
    (do-if-atts
        (λ
            (atts)
            (string-join
                (dict-map
                    atts
                    (λ
                        (a b)
                        (let
                            ((starts-with (string-ref (symbol->string a) 0)))
                            (if
                                (char=? starts-with #\_)
                                ""
                                (format
                                    " ~a=\"~a\""
                                    a
                                    b)))))
                ""))
        entry
        ""))

(define
    (generate-entry-html template head data level)
    (~a
        (conditional-indent template level)
        (format
            "<~a~a>~a</~a>"
            head
            (replace-map (generate-attribute-html template) data)
            (let
                ((content (car (cdr template))))
                (if
                    (list? content)
                    (~a (template->html content data (+ level 1)) (conditional-indent content level))
                    (replace-map content data)))
            head)))

(define
    (template->html template data [level 0])
    (if
        (list? template)
        (if
            (null? template)
            ""
            (let
                ((head (car template)))
                (if
                    (list? head)
                    (string-join (map (λ (x) (template->html x data level)) template))
                    (generate-entry-html template head data level))))
        template))

(define
    (compose-article article [level 0])
    (template->html article-template article level))

(define
    data
    '(
        (title . "Entrepreneurs’ Connection")
        (desc . "Here we provide some articles offering advice and other help to entrepreneurs, whether you’re part of a top 500 company or starting your own small, local business, to help you succeed.")
        (categories . (
            ((title . "General")
                (articles . (
                    ((title . "What Is an Entrepreneur?")
                        (desc . "Learn the definition of an entrepreneur and the characteristics that make them succeed in business…")
                        (url . "https://www.seattlesouthsidechamber.com/what-is-an-entrepreneur/"))
                    ((title . "Marketing Strategy")
                        (desc . "There are many types of marketing strategies and the best way to market your business will vary from business to business depending on many different aspects, such as who your target audience is and what kind of product or service you are selling…")
                        (url . "https://www.seattlesouthsidechamber.com/marketing-strategy/"))
                    ((title . "Branding")
                        (desc . "Branding is your business’s identity, the culmination of everything your customers think of your company, so it is important that your business maintains a brand that fits their ideal culture and demographic…")
                        (url . "https://www.seattlesouthsidechamber.com/branding/")))))
            ((title . "People")
                (articles . (
                    ((title . "Diversity and Inclusion")
                        (desc . "Many business leaders wonder what they can do to make their companies more diverse…")
                        (url . "https://www.seattlesouthsidechamber.com/diversity-and-inclusion/"))
                    ((title . "How to Handle Work Stress")
                        (desc . "Everyone in your business, both employees and owners, can find themselves overwhelmed by stress, due to either work or home-life changes or a mixture of both…")
                        (url . "https://www.seattlesouthsidechamber.com/how-to-handle-work-stress/")))))
            ((title . "Emergencies")
                (articles . (
                    ((title . "SBA Disaster Loans")
                        (desc . "n our current economic troubles some small businesses may need assistance, especially those hit by COVID-19. In addition to loans related to coronavirus, the SBA also offers loans for natural disasters, such as floods…")
                        (url . "https://www.seattlesouthsidechamber.com/sba-disaster-loans/"))
                    ((title . "Prepare for Emergencies")
                        (desc . "Since you can never predict when a natural disaster will strike, it is a good idea to prepare your business to protect it as much as you can before a disaster happens…")
                        (url . "https://www.seattlesouthsidechamber.com/prepare-for-emergencies/")))))
            ((title . "Misc. Tips")
                (articles . (
                    ((title . "Add Your Business to Google")
                        (desc . "Since Google is the leading search engine in the world and the main way most find websites, it is vital that you list your business on Google to help potential customers find your business…")
                        (url . "https://www.seattlesouthsidechamber.com/add-your-business-to-google/"))
                    ((title . "RFP Marketing")
                        (desc . "RFP stands for “real-time personalization” and refers to content that customizes itself for different kinds of people…")
                        (url . "https://www.seattlesouthsidechamber.com/rfp-marketing/"))
                    ((title . "Trade Shows")
                        (desc . "Trade shows can be useful for businesses and professionals alike, as a way for professionals to find jobs and for businesses to find employees and customers…")
                        (url . "https://www.seattlesouthsidechamber.com/trade-shows/"))
                    ((title . "Contracting with the Government")
                        (desc . "If your company is selling products or services that may be useful for the federal or state governments, it may be a great opportunity to make them a customer…")
                        (url . "https://www.seattlesouthsidechamber.com/contracting-with-the-government/")))))))))

(define
    (slugify text)
    (regexp-replace
        #px"\\s"
        (regexp-replace
            #px"[^a-zA-Z0=9]"
            (string-downcase text)
            "")
        "-"))

(define
    (compile-category category)
    (dict-set
            (dict-set
                category
                'content
                (string-join
                    (map
                        (λ (a) (compose-article a 2))
                        (dict-ref category 'articles))
                        ""))
            'id
            (slugify (dict-ref category 'title))))

(define
    (compile-categories categories)
    (string-join (map compose-category categories) ""))

(define
    (compile-category-link category)
    (format "<a href=\"#~a\">~a</a>" (dict-ref category 'id) (dict-ref category 'title)))

(define
    (compile-category-links categories)
    (string-join (map compile-category-link categories) " | "))

(define
    (compile-data data)
    (let
        ((root
            (dict-set
                data
                'categories
                (map compile-category (dict-ref data 'categories)))))
        (dict-set
            (dict-set
                root
                'catcontent
                (compile-categories (dict-ref root 'categories)))
            'catlinks
            (compile-category-links (dict-ref root 'categories)))))

(define
    (compose-category category)
    (let
        ((compiled-cat (compile-category category)))
        (template->html
            category-template
            compiled-cat)))

(define
    (compose-root data)
    (let
        ((root (compile-data data)))
        (template->html root-template root)))

(echo (compose-root data))