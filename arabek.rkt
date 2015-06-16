#lang racket

(define (one-of seq) 
  (cond
    [(vector? seq) (vector-ref seq (random (vector-length seq)))]
    [(list? seq) (list-ref seq (random (length seq)))]
    [(string? seq) (one-of (string->list seq))]
    [else seq]))

(struct Powerset (acolyte blessings) #:transparent)
(struct God (focus terrain appearance gender personality values 
                   base-powers new-powers) #:transparent)

(define perslist 
  '(Accessible Abrasive Abrupt Absentminded Active Adaptable Admirable Adventurous Aggressive Agonizing Agreeable Aimless Airy Alert Allocentric Aloof Ambitious Amiable Amoral Amusing Angry Anticipative Anxious Apathetic Appreciative Arbitrary Argumentative Arrogant Artful Articulate Artificial Ascetic Asocial Aspiring Assertive Astigmatic Athletic Attractive Authoritarian
               Balanced Barbaric Benevolent Bewildered Big-thinking Boisterous Bizarre Bland Blunt Boyish Breezy Brilliant Brittle Brutal Businesslike Busy
               Calculating Callous Calm Cantakerous Capable Captivating Careless Caring Casual Cautious Challenging Charismatic Charming Charmless Cheerful Childish Chummy Circumspect Clean Clear-headed Clever Clumsy Coarse Cold Colorful Colorless Companionly Compassionate Competitive Complacent Complaintive Complex Compulsive Conceited Conciliatory Condemnatory Confident Confidential Conformist Confused Conscientious Conservative Considerate Constant Contemplative Contemptible Contradictory Conventional Cooperative Courageous Courteous Cowardly Crafty Crass Crazy Creative Cerebral Criminal Crisp Critical Crude Cruel Cultured Curious Cute Cynical
               Daring Debonair Decadent Deceitful Decent Deceptive Decisive Dedicated Deep Delicate Demanding Dependent Desperate Destructive Determined Devious Difficult Dignified Directed Dirty Disciplined Disconcerting Discontented Discouraging Discourteous Discreet Dishonest Disloyal Disobedient Disorderly Disorganized Disputatious Disrespectful Disruptive Dissolute Dissonant Distractible Disturbing Dogmatic Dominating Domineering Dramatic Dreamy Driving Droll Dry Dull Dutiful Dynamic
               Earnest Earthy Easily-Discouraged Ebullient Educated Effeminate Efficient Egocentric Elegant Eloquent Emotional Empathetic Energetic Enervated Enigmatic Enthusiastic Envious Erratic Escapist Esthetic Excitable Exciting Expedient Experimental Extraordinary Extravagant Extreme
               Fair Faithful Faithless Familial Fanatical Fanciful Farsighted Fatalistic Fawning Fearful Felicific Fickle Fiery Firm Fixed Flamboyant Flexible Focused Folksy Foolish Forceful Forgetful Forgiving Formal Forthright Fraudulent Freethinking Freewheeling Friendly Frightening Frivolous Frugal Fun-loving
               Gallant Generous Gentle Genuine Glamorous Gloomy Good-natured Graceless Gracious Grand Greedy Grim Guileless Gullible
               Hardworking Hateful Haughty Healthy Hearty Hedonistic Helpful Heroic Hesitant Hidebound High-handed High-minded High-spirited Honest Honorable Hostile Humble Humorous Hurried Hypnotic
               Iconoclastic Idealistic Idiosyncratic Ignorant Imaginative Imitative Impassive Impatient Impersonal Impractical Impressionable Impressive Imprudent Impulsive Incisive Inconsiderate Incorruptible Incurious Indecisive Independent Individualistic Indulgent Inert Inhibited Innovative Inoffensive Insecure Insensitive Insightful Insincere Insouciant Insulting Intelligent Intense Intolerant Intuitive Invisible Invulnerable Irascible Irrational Irreligious Irresponsible Irreverent Irritable
               Kind Knowledge
               Lazy Leaderly Leisurely Liberal Libidinous Logical Loquacious Lovable Loyal Lyrical
               Magnanimous Malicious Mannered Mannerless Many-sided Masculine Maternal Meticulous Mature Mawkish Mealymouthed Mechanical Meddlesome Melancholic Mellow Meretricious Messy Methodical Miserable Miserly Misguided Mistaken Moderate Modern Modest Money-minded Monstrous Moody Moralistic Morbid Muddle-headed Multi-leveled Mystical
               NaiveNarcissistic Narrow Narrow-minded Natty Neat Negativistic Neglectful Neurotic Neutral Nihilistic Nonauthoritarian Noncommittal Noncompetitive
               Obedient Objective Obnoxious Observant Obsessive Obvious Odd Offhand Old-fashioned One-dimensional One-sided Open Opinionated Opportunistic Oppressed Optimistic Orderly Ordinary Organized Original Outrageous Outspoken Overimaginative
               Painstaking Paranoid Passionate Passive Paternalistic Patient Patriotic Peaceful Pedantic Perceptive Perfectionist Personable Persuasive Perverse Petty Pharissical Phlegmatic Physical Placid Planful Playful Plodding Polished Political Pompous Popular Possessive Power-hungry Practical Precise Predatory Predictable Prejudiced Preoccupied Presumptuous Pretentious Prim Principled Private Procrastinating Profligate Profound Progressive Protean Protective Proud Providential Provocative Prudent Pruposeful Pugnacious Punctual Pure Puritanical
               Questioning Quiet Quirky
               Rational Reactionary Reactive Realistic Reflective Regimental Regretful Relaxed Reliable Religious Repentant Repressed Resentful Reserved Resourceful Respectful Responsible Responsive Restrained Retiring Reverential Ridiculous Rigid Ritualistic Romantic Rowdy Ruined Rustic
               Sadistic Sage Sanctimonious Sane Sarcastic Scheming Scholarly Scornful Scrupulous Secretive Secure Sedentary Self-conscious Self-critical Self-defacing Self-denying Self-indulgent Self-reliant Self-sufficent Selfish Selfless Sensitive Sensual Sentimental Seraphic Serious Shallow Sharing Shortsighted Shrewd Shy Silly Simple Single-minded Skeptical Skillful Sloppy Slow Sly Small-thinking Smooth Sober Sociable Soft Softheaded Solemn Solid Solitary Sophisticated Sordid Spontaneous Sporting Stable Steadfast Steady Steely Stern Stiff Stoic Stolid Strict Strong Strong-willed Stubborn Studious Stupid Stylish Suave Subjective Submissive Subtle Superficial Superstitious Surprising Suspicious Sweet Sympathetic Systematic
               Tactless Tasteful Tasteless Teacherly Tense Thievish Thorough Thoughtless Tidy Timid Tolerant Tough Tractable Transparent Treacherous Trendy Troublesome Trusting
               Unaggressive Unambitious Unappreciative Uncaring Unceremonious Unchanging Uncharitable Uncomplaining Unconvincing Uncooperative Uncreative Uncritical Unctuous Undemanding Understanding Undisciplined Undogmatic Unfathomable Unfoolable Unfriendly Ungrateful Unhealthy Unhurried Unimaginative Unimpressive Uninhibited Unlovable Unpatriotic Unpolished Unpredictable Unprincipled Unrealistic Unreflective Unreliable Unreligious Unrestrained Unself-critical Unsentimental Unstable Upright Urbane
               Vacuous Vague Venal Venomous Venturesome Vindictive Vivacious Vulnerable
               Warm Weak Weak-willed Well-bred Well-meaning Well-read Well-rounded Whimsical
               Willful Winning Wise Wishful Witty
               Youthful
               Zany))

(define powerlist
  '(Sensory Environmental Military Defensive Helps-others Protection Mythical-feat Mental-upgrade Physical-upgrade Decrease-vulnerability Utility))

(define focuslist
  '(Flowery Nature Primitive Holy Magic Peace Ugly Old Subordinate Leader New Domestic Mythic Artifice Color Mystery Romantic Assertive Aquatic Protect Thought Wild Earth Lake Balance Boundary Dance Darkness Light Order Festival Family Fire Food Freedom Games Luck Music Sky Silence Trade Travel Truth Wealth))

(define biomelist
  '(Swamp Marsh Taiga Forest-coniferous Forest-broadleaf Forest-tropical Grassland Desert Badlands Ocean Lake Mountain Glacier Tundra Savannah Shrubland Hills))

(define genderlist '(Male Female))

(define (gen-powerset)
  (Powerset (one-of powerlist) (build-list 6 (λ (n) (one-of powerlist)))))

(define (gen-god)
  (God (one-of focuslist) (one-of biomelist) (one-of perslist) (one-of genderlist) 
       (build-list 3 (λ (n) (one-of perslist))) (build-list 3 (λ (n) (one-of perslist)))
       (gen-powerset) (gen-powerset)))

(define (string-up l)
  (apply string-append (map (λ (p) (string-append (symbol->string p) " ")) l)))

(define (print-god g)
  (displayln (string-append "Focus: " (symbol->string (God-focus g))))
  (displayln (string-append "Terrain: " (symbol->string (God-terrain g))))
  (displayln (string-append "Appearance: " (symbol->string (God-appearance g))))
  (displayln (string-append "Gender: " (symbol->string (God-gender g))))
  (displayln (string-append "Personality: " (string-up (God-personality g))))
  (displayln (string-append "Values: " (string-up (God-values g))))
  (displayln (string-append "Acolyte power 1: " (symbol->string (Powerset-acolyte (God-base-powers g)))))
  (displayln (string-append "Blessing set 1: " (string-up (Powerset-blessings (God-base-powers g)))))
  (displayln (string-append "Acolyte power 2: " (symbol->string (Powerset-acolyte (God-new-powers g)))))
  (displayln (string-append "Blessing set 2: " (string-up (Powerset-blessings (God-new-powers g))))))
