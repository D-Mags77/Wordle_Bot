#include <iostream>
#include <stdio.h>
#include <string>
#include <vector>
#include <random>
#include <map>
#include <numeric> 
#include <algorithm>
#include <chrono>
#include <unordered_map> 
#include <iomanip>  
#include <utility>
#include <iterator>
#include <fstream>
#include <cmath>
#include "Probabilties.cpp";

using namespace std;
using namespace std::chrono;

vector<string> WordListVector = {"aback",
"abase",
"abate",
"abbey",
"abbot",
"abhor",
"abide",
"abled",
"abode",
"abort",
"about",
"above",
"abuse",
"abyss",
"acorn",
"acrid",
"actor",
"acute",
"adage",
"adapt",
"adept",
"admin",
"admit",
"adobe",
"adopt",
"adore",
"adorn",
"adult",
"affix",
"afire",
"afoot",
"afoul",
"after",
"again",
"agape",
"agate",
"agent",
"agile",
"aging",
"aglow",
"agony",
"agree",
"ahead",
"aider",
"aisle",
"alarm",
"album",
"alert",
"algae",
"alibi",
"alien",
"align",
"alike",
"alive",
"allay",
"alley",
"allot",
"allow",
"alloy",
"aloft",
"alone",
"along",
"aloof",
"aloud",
"alpha",
"altar",
"alter",
"amass",
"amaze",
"amber",
"amble",
"amend",
"amiss",
"amity",
"among",
"ample",
"amply",
"amuse",
"angel",
"anger",
"angle",
"angry",
"angst",
"anime",
"ankle",
"annex",
"annoy",
"annul",
"anode",
"antic",
"anvil",
"aorta",
"apart",
"aphid",
"aping",
"apnea",
"apple",
"apply",
"apron",
"aptly",
"arbor",
"ardor",
"arena",
"argue",
"arise",
"armor",
"aroma",
"arose",
"array",
"arrow",
"arson",
"artsy",
"ascot",
"ashen",
"aside",
"askew",
"assay",
"asset",
"atoll",
"atone",
"attic",
"audio",
"audit",
"augur",
"aunty",
"avail",
"avert",
"avian",
"avoid",
"await",
"awake",
"award",
"aware",
"awash",
"awful",
"awoke",
"axial",
"axiom",
"axion",
"azure",
"bacon",
"badge",
"badly",
"bagel",
"baggy",
"baker",
"baler",
"balmy",
"banal",
"banjo",
"barge",
"baron",
"basal",
"basic",
"basil",
"basin",
"basis",
"baste",
"batch",
"bathe",
"baton",
"batty",
"bawdy",
"bayou",
"beach",
"beady",
"beard",
"beast",
"beech",
"beefy",
"befit",
"began",
"begat",
"beget",
"begin",
"begun",
"being",
"belch",
"belie",
"belle",
"belly",
"below",
"bench",
"beret",
"berry",
"berth",
"beset",
"betel",
"bevel",
"bezel",
"bible",
"bicep",
"biddy",
"bigot",
"bilge",
"billy",
"binge",
"bingo",
"biome",
"birch",
"birth",
"bison",
"bitty",
"black",
"blade",
"blame",
"bland",
"blank",
"blare",
"blast",
"blaze",
"bleak",
"bleat",
"bleed",
"bleep",
"blend",
"bless",
"blimp",
"blind",
"blink",
"bliss",
"blitz",
"bloat",
"block",
"bloke",
"blond",
"blood",
"bloom",
"blown",
"bluer",
"bluff",
"blunt",
"blurb",
"blurt",
"blush",
"board",
"boast",
"bobby",
"boney",
"bongo",
"bonus",
"booby",
"boost",
"booth",
"booty",
"booze",
"boozy",
"borax",
"borne",
"bosom",
"bossy",
"botch",
"bough",
"boule",
"bound",
"bowel",
"boxer",
"brace",
"braid",
"brain",
"brake",
"brand",
"brash",
"brass",
"brave",
"bravo",
"brawl",
"brawn",
"bread",
"break",
"breed",
"briar",
"bribe",
"brick",
"bride",
"brief",
"brine",
"bring",
"brink",
"briny",
"brisk",
"broad",
"broil",
"broke",
"brood",
"brook",
"broom",
"broth",
"brown",
"brunt",
"brush",
"brute",
"buddy",
"budge",
"buggy",
"bugle",
"build",
"built",
"bulge",
"bulky",
"bully",
"bunch",
"bunny",
"burly",
"burnt",
"burst",
"bused",
"bushy",
"butch",
"butte",
"buxom",
"buyer",
"bylaw",
"cabal",
"cabby",
"cabin",
"cable",
"cacao",
"cache",
"cacti",
"caddy",
"cadet",
"cagey",
"cairn",
"camel",
"cameo",
"canal",
"candy",
"canny",
"canoe",
"canon",
"caper",
"caput",
"carat",
"cargo",
"carol",
"carry",
"carve",
"caste",
"catch",
"cater",
"catty",
"caulk",
"cause",
"cavil",
"cease",
"cedar",
"cello",
"chafe",
"chaff",
"chain",
"chair",
"chalk",
"champ",
"chant",
"chaos",
"chard",
"charm",
"chart",
"chase",
"chasm",
"cheap",
"cheat",
"check",
"cheek",
"cheer",
"chess",
"chest",
"chick",
"chide",
"chief",
"child",
"chili",
"chill",
"chime",
"china",
"chirp",
"chock",
"choir",
"choke",
"chord",
"chore",
"chose",
"chuck",
"chump",
"chunk",
"churn",
"chute",
"cider",
"cigar",
"cinch",
"circa",
"civic",
"civil",
"clack",
"claim",
"clamp",
"clang",
"clank",
"clash",
"clasp",
"class",
"clean",
"clear",
"cleat",
"cleft",
"clerk",
"click",
"cliff",
"climb",
"cling",
"clink",
"cloak",
"clock",
"clone",
"close",
"cloth",
"cloud",
"clout",
"clove",
"clown",
"cluck",
"clued",
"clump",
"clung",
"coach",
"coast",
"cobra",
"cocoa",
"colon",
"color",
"comet",
"comfy",
"comic",
"comma",
"conch",
"condo",
"conic",
"copse",
"coral",
"corer",
"corny",
"couch",
"cough",
"could",
"count",
"coupe",
"court",
"coven",
"cover",
"covet",
"covey",
"cower",
"coyly",
"crack",
"craft",
"cramp",
"crane",
"crank",
"crash",
"crass",
"crate",
"crave",
"crawl",
"craze",
"crazy",
"creak",
"cream",
"credo",
"creed",
"creek",
"creep",
"creme",
"crepe",
"crept",
"cress",
"crest",
"crick",
"cried",
"crier",
"crime",
"crimp",
"crisp",
"croak",
"crock",
"crone",
"crony",
"crook",
"cross",
"croup",
"crowd",
"crown",
"crude",
"cruel",
"crumb",
"crump",
"crush",
"crust",
"crypt",
"cubic",
"cumin",
"curio",
"curly",
"curry",
"curse",
"curve",
"curvy",
"cutie",
"cyber",
"cycle",
"cynic",
"daddy",
"daily",
"dairy",
"daisy",
"dally",
"dance",
"dandy",
"datum",
"daunt",
"dealt",
"death",
"debar",
"debit",
"debug",
"debut",
"decal",
"decay",
"decor",
"decoy",
"decry",
"defer",
"deign",
"deity",
"delay",
"delta",
"delve",
"demon",
"demur",
"denim",
"dense",
"depot",
"depth",
"derby",
"deter",
"detox",
"deuce",
"devil",
"diary",
"dicey",
"digit",
"dilly",
"dimly",
"diner",
"dingo",
"dingy",
"diode",
"dirge",
"dirty",
"disco",
"ditch",
"ditto",
"ditty",
"diver",
"dizzy",
"dodge",
"dodgy",
"dogma",
"doing",
"dolly",
"donor",
"donut",
"dopey",
"doubt",
"dough",
"dowdy",
"dowel",
"downy",
"dowry",
"dozen",
"draft",
"drain",
"drake",
"drama",
"drank",
"drape",
"drawl",
"drawn",
"dread",
"dream",
"dress",
"dried",
"drier",
"drift",
"drill",
"drink",
"drive",
"droit",
"droll",
"drone",
"drool",
"droop",
"dross",
"drove",
"drown",
"druid",
"drunk",
"dryer",
"dryly",
"duchy",
"dully",
"dummy",
"dumpy",
"dunce",
"dusky",
"dusty",
"dutch",
"duvet",
"dwarf",
"dwell",
"dwelt",
"dying",
"eager",
"eagle",
"early",
"earth",
"easel",
"eaten",
"eater",
"ebony",
"eclat",
"edict",
"edify",
"eerie",
"egret",
"eight",
"eject",
"eking",
"elate",
"elbow",
"elder",
"elect",
"elegy",
"elfin",
"elide",
"elite",
"elope",
"elude",
"email",
"embed",
"ember",
"emcee",
"empty",
"enact",
"endow",
"enema",
"enemy",
"enjoy",
"ennui",
"ensue",
"enter",
"entry",
"envoy",
"epoch",
"epoxy",
"equal",
"equip",
"erase",
"erect",
"erode",
"error",
"erupt",
"essay",
"ester",
"ether",
"ethic",
"ethos",
"etude",
"evade",
"event",
"every",
"evict",
"evoke",
"exact",
"exalt",
"excel",
"exert",
"exile",
"exist",
"expel",
"extol",
"extra",
"exult",
"eying",
"fable",
"facet",
"faint",
"fairy",
"faith",
"false",
"fancy",
"fanny",
"farce",
"fatal",
"fatty",
"fault",
"fauna",
"favor",
"feast",
"fecal",
"feign",
"fella",
"felon",
"femme",
"femur",
"fence",
"feral",
"ferry",
"fetal",
"fetch",
"fetid",
"fetus",
"fever",
"fewer",
"fiber",
"ficus",
"field",
"fiend",
"fiery",
"fifth",
"fifty",
"fight",
"filer",
"filet",
"filly",
"filmy",
"filth",
"final",
"finch",
"finer",
"first",
"fishy",
"fixer",
"fizzy",
"fjord",
"flack",
"flail",
"flair",
"flake",
"flaky",
"flame",
"flank",
"flare",
"flash",
"flask",
"fleck",
"fleet",
"flesh",
"flick",
"flier",
"fling",
"flint",
"flirt",
"float",
"flock",
"flood",
"floor",
"flora",
"floss",
"flour",
"flout",
"flown",
"fluff",
"fluid",
"fluke",
"flume",
"flung",
"flunk",
"flush",
"flute",
"flyer",
"foamy",
"focal",
"focus",
"foggy",
"foist",
"folio",
"folly",
"foray",
"force",
"forge",
"forgo",
"forte",
"forth",
"forty",
"forum",
"found",
"foyer",
"frail",
"frame",
"frank",
"fraud",
"freak",
"freed",
"freer",
"fresh",
"friar",
"fried",
"frill",
"frisk",
"fritz",
"frock",
"frond",
"front",
"frost",
"froth",
"frown",
"froze",
"fruit",
"fudge",
"fugue",
"fully",
"fungi",
"funky",
"funny",
"furor",
"furry",
"fussy",
"fuzzy",
"gaffe",
"gaily",
"gamer",
"gamma",
"gamut",
"gassy",
"gaudy",
"gauge",
"gaunt",
"gauze",
"gavel",
"gawky",
"gayer",
"gayly",
"gazer",
"gecko",
"geeky",
"geese",
"genie",
"genre",
"ghost",
"ghoul",
"giant",
"giddy",
"gipsy",
"girly",
"girth",
"given",
"giver",
"glade",
"gland",
"glare",
"glass",
"glaze",
"gleam",
"glean",
"glide",
"glint",
"gloat",
"globe",
"gloom",
"glory",
"gloss",
"glove",
"glyph",
"gnash",
"gnome",
"godly",
"going",
"golem",
"golly",
"gonad",
"goner",
"goody",
"gooey",
"goofy",
"goose",
"gorge",
"gouge",
"gourd",
"grace",
"grade",
"graft",
"grail",
"grain",
"grand",
"grant",
"grape",
"graph",
"grasp",
"grass",
"grate",
"grave",
"gravy",
"graze",
"great",
"greed",
"green",
"greet",
"grief",
"grill",
"grime",
"grimy",
"grind",
"gripe",
"groan",
"groin",
"groom",
"grope",
"gross",
"group",
"grout",
"grove",
"growl",
"grown",
"gruel",
"gruff",
"grunt",
"guard",
"guava",
"guess",
"guest",
"guide",
"guild",
"guile",
"guilt",
"guise",
"gulch",
"gully",
"gumbo",
"gummy",
"guppy",
"gusto",
"gusty",
"gypsy",
"habit",
"hairy",
"halve",
"handy",
"happy",
"hardy",
"harem",
"harpy",
"harry",
"harsh",
"haste",
"hasty",
"hatch",
"hater",
"haunt",
"haute",
"haven",
"havoc",
"hazel",
"heady",
"heard",
"heart",
"heath",
"heave",
"heavy",
"hedge",
"hefty",
"heist",
"helix",
"hello",
"hence",
"heron",
"hilly",
"hinge",
"hippo",
"hippy",
"hitch",
"hoard",
"hobby",
"hoist",
"holly",
"homer",
"honey",
"honor",
"horde",
"horny",
"horse",
"hotel",
"hotly",
"hound",
"house",
"hovel",
"hover",
"howdy",
"human",
"humid",
"humor",
"humph",
"humus",
"hunch",
"hunky",
"hurry",
"husky",
"hussy",
"hutch",
"hydro",
"hyena",
"hymen",
"hyper",
"icily",
"icing",
"ideal",
"idiom",
"idiot",
"idler",
"idyll",
"igloo",
"iliac",
"image",
"imbue",
"impel",
"imply",
"inane",
"inbox",
"incur",
"index",
"inept",
"inert",
"infer",
"ingot",
"inlay",
"inlet",
"inner",
"input",
"inter",
"intro",
"ionic",
"irate",
"irony",
"islet",
"issue",
"itchy",
"ivory",
"jaunt",
"jazzy",
"jelly",
"jerky",
"jetty",
"jewel",
"jiffy",
"joint",
"joist",
"joker",
"jolly",
"joust",
"judge",
"juice",
"juicy",
"jumbo",
"jumpy",
"junta",
"junto",
"juror",
"kappa",
"karma",
"kayak",
"kebab",
"khaki",
"kinky",
"kiosk",
"kitty",
"knack",
"knave",
"knead",
"kneed",
"kneel",
"knelt",
"knife",
"knock",
"knoll",
"known",
"koala",
"krill",
"label",
"labor",
"laden",
"ladle",
"lager",
"lance",
"lanky",
"lapel",
"lapse",
"large",
"larva",
"lasso",
"latch",
"later",
"lathe",
"latte",
"laugh",
"layer",
"leach",
"leafy",
"leaky",
"leant",
"leapt",
"learn",
"lease",
"leash",
"least",
"leave",
"ledge",
"leech",
"leery",
"lefty",
"legal",
"leggy",
"lemon",
"lemur",
"leper",
"level",
"lever",
"libel",
"liege",
"light",
"liken",
"lilac",
"limbo",
"limit",
"linen",
"liner",
"lingo",
"lipid",
"lithe",
"liver",
"livid",
"llama",
"loamy",
"loath",
"lobby",
"local",
"locus",
"lodge",
"lofty",
"logic",
"login",
"loopy",
"loose",
"lorry",
"loser",
"louse",
"lousy",
"lover",
"lower",
"lowly",
"loyal",
"lucid",
"lucky",
"lumen",
"lumpy",
"lunar",
"lunch",
"lunge",
"lupus",
"lurch",
"lurid",
"lusty",
"lying",
"lymph",
"lyric",
"macaw",
"macho",
"macro",
"madam",
"madly",
"mafia",
"magic",
"magma",
"maize",
"major",
"maker",
"mambo",
"mamma",
"mammy",
"manga",
"mange",
"mango",
"mangy",
"mania",
"manic",
"manly",
"manor",
"maple",
"march",
"marry",
"marsh",
"mason",
"masse",
"match",
"matey",
"mauve",
"maxim",
"maybe",
"mayor",
"mealy",
"meant",
"meaty",
"mecca",
"medal",
"media",
"medic",
"melee",
"melon",
"mercy",
"merge",
"merit",
"merry",
"metal",
"meter",
"metro",
"micro",
"midge",
"midst",
"might",
"milky",
"mimic",
"mince",
"miner",
"minim",
"minor",
"minty",
"minus",
"mirth",
"miser",
"missy",
"mocha",
"modal",
"model",
"modem",
"mogul",
"moist",
"molar",
"moldy",
"money",
"month",
"moody",
"moose",
"moral",
"moron",
"morph",
"mossy",
"motel",
"motif",
"motor",
"motto",
"moult",
"mound",
"mount",
"mourn",
"mouse",
"mouth",
"mover",
"movie",
"mower",
"mucky",
"mucus",
"muddy",
"mulch",
"mummy",
"munch",
"mural",
"murky",
"mushy",
"music",
"musky",
"musty",
"myrrh",
"nadir",
"naive",
"nanny",
"nasal",
"nasty",
"natal",
"naval",
"navel",
"needy",
"neigh",
"nerdy",
"nerve",
"never",
"newer",
"newly",
"nicer",
"niche",
"niece",
"night",
"ninja",
"ninny",
"ninth",
"noble",
"nobly",
"noise",
"noisy",
"nomad",
"noose",
"north",
"nosey",
"notch",
"novel",
"nudge",
"nurse",
"nutty",
"nylon",
"nymph",
"oaken",
"obese",
"occur",
"ocean",
"octal",
"octet",
"odder",
"oddly",
"offal",
"offer",
"often",
"olden",
"older",
"olive",
"ombre",
"omega",
"onion",
"onset",
"opera",
"opine",
"opium",
"optic",
"orbit",
"order",
"organ",
"other",
"otter",
"ought",
"ounce",
"outdo",
"outer",
"outgo",
"ovary",
"ovate",
"overt",
"ovine",
"ovoid",
"owing",
"owner",
"oxide",
"ozone",
"paddy",
"pagan",
"paint",
"paler",
"palsy",
"panel",
"panic",
"pansy",
"papal",
"paper",
"parer",
"parka",
"parry",
"parse",
"party",
"pasta",
"paste",
"pasty",
"patch",
"patio",
"patsy",
"patty",
"pause",
"payee",
"payer",
"peace",
"peach",
"pearl",
"pecan",
"pedal",
"penal",
"pence",
"penne",
"penny",
"perch",
"peril",
"perky",
"pesky",
"pesto",
"petal",
"petty",
"phase",
"phone",
"phony",
"photo",
"piano",
"picky",
"piece",
"piety",
"piggy",
"pilot",
"pinch",
"piney",
"pinky",
"pinto",
"piper",
"pique",
"pitch",
"pithy",
"pivot",
"pixel",
"pixie",
"pizza",
"place",
"plaid",
"plain",
"plait",
"plane",
"plank",
"plant",
"plate",
"plaza",
"plead",
"pleat",
"plied",
"plier",
"pluck",
"plumb",
"plume",
"plump",
"plunk",
"plush",
"poesy",
"point",
"poise",
"poker",
"polar",
"polka",
"polyp",
"pooch",
"poppy",
"porch",
"poser",
"posit",
"posse",
"pouch",
"pound",
"pouty",
"power",
"prank",
"prawn",
"preen",
"press",
"price",
"prick",
"pride",
"pried",
"prime",
"primo",
"print",
"prior",
"prism",
"privy",
"prize",
"probe",
"prone",
"prong",
"proof",
"prose",
"proud",
"prove",
"prowl",
"proxy",
"prude",
"prune",
"psalm",
"pubic",
"pudgy",
"puffy",
"pulpy",
"pulse",
"punch",
"pupil",
"puppy",
"puree",
"purer",
"purge",
"purse",
"pushy",
"putty",
"pygmy",
"quack",
"quail",
"quake",
"qualm",
"quark",
"quart",
"quash",
"quasi",
"queen",
"queer",
"quell",
"query",
"quest",
"queue",
"quick",
"quiet",
"quill",
"quilt",
"quirk",
"quite",
"quota",
"quote",
"quoth",
"rabbi",
"rabid",
"racer",
"radar",
"radii",
"radio",
"rainy",
"raise",
"rajah",
"rally",
"ralph",
"ramen",
"ranch",
"randy",
"range",
"rapid",
"rarer",
"raspy",
"ratio",
"ratty",
"raven",
"rayon",
"razor",
"reach",
"react",
"ready",
"realm",
"rearm",
"rebar",
"rebel",
"rebus",
"rebut",
"recap",
"recur",
"recut",
"reedy",
"refer",
"refit",
"regal",
"rehab",
"reign",
"relax",
"relay",
"relic",
"remit",
"renal",
"renew",
"repay",
"repel",
"reply",
"rerun",
"reset",
"resin",
"retch",
"retro",
"retry",
"reuse",
"revel",
"revue",
"rhino",
"rhyme",
"rider",
"ridge",
"rifle",
"right",
"rigid",
"rigor",
"rinse",
"ripen",
"riper",
"risen",
"riser",
"risky",
"rival",
"river",
"rivet",
"roach",
"roast",
"robin",
"robot",
"rocky",
"rodeo",
"roger",
"rogue",
"roomy",
"roost",
"rotor",
"rouge",
"rough",
"round",
"rouse",
"route",
"rover",
"rowdy",
"rower",
"royal",
"ruddy",
"ruder",
"rugby",
"ruler",
"rumba",
"rumor",
"rupee",
"rural",
"rusty",
"sadly",
"safer",
"saint",
"salad",
"sally",
"salon",
"salsa",
"salty",
"salve",
"salvo",
"sandy",
"saner",
"sappy",
"sassy",
"satin",
"satyr",
"sauce",
"saucy",
"sauna",
"saute",
"savor",
"savoy",
"savvy",
"scald",
"scale",
"scalp",
"scaly",
"scamp",
"scant",
"scare",
"scarf",
"scary",
"scene",
"scent",
"scion",
"scoff",
"scold",
"scone",
"scoop",
"scope",
"score",
"scorn",
"scour",
"scout",
"scowl",
"scram",
"scrap",
"scree",
"screw",
"scrub",
"scrum",
"scuba",
"sedan",
"seedy",
"segue",
"seize",
"semen",
"sense",
"sepia",
"serif",
"serum",
"serve",
"setup",
"seven",
"sever",
"sewer",
"shack",
"shade",
"shady",
"shaft",
"shake",
"shaky",
"shale",
"shall",
"shalt",
"shame",
"shank",
"shape",
"shard",
"share",
"shark",
"sharp",
"shave",
"shawl",
"shear",
"sheen",
"sheep",
"sheer",
"sheet",
"sheik",
"shelf",
"shell",
"shied",
"shift",
"shine",
"shiny",
"shire",
"shirk",
"shirt",
"shoal",
"shock",
"shone",
"shook",
"shoot",
"shore",
"shorn",
"short",
"shout",
"shove",
"shown",
"showy",
"shrew",
"shrub",
"shrug",
"shuck",
"shunt",
"shush",
"shyly",
"siege",
"sieve",
"sight",
"sigma",
"silky",
"silly",
"since",
"sinew",
"singe",
"siren",
"sissy",
"sixth",
"sixty",
"skate",
"skier",
"skiff",
"skill",
"skimp",
"skirt",
"skulk",
"skull",
"skunk",
"slack",
"slain",
"slang",
"slant",
"slash",
"slate",
"sleek",
"sleep",
"sleet",
"slept",
"slice",
"slick",
"slide",
"slime",
"slimy",
"sling",
"slink",
"sloop",
"slope",
"slosh",
"sloth",
"slump",
"slung",
"slunk",
"slurp",
"slush",
"slyly",
"smack",
"small",
"smart",
"smash",
"smear",
"smell",
"smelt",
"smile",
"smirk",
"smite",
"smith",
"smock",
"smoke",
"smoky",
"smote",
"snack",
"snail",
"snake",
"snaky",
"snare",
"snarl",
"sneak",
"sneer",
"snide",
"sniff",
"snipe",
"snoop",
"snore",
"snort",
"snout",
"snowy",
"snuck",
"snuff",
"soapy",
"sober",
"soggy",
"solar",
"solid",
"solve",
"sonar",
"sonic",
"sooth",
"sooty",
"sorry",
"sound",
"south",
"sower",
"space",
"spade",
"spank",
"spare",
"spark",
"spasm",
"spawn",
"speak",
"spear",
"speck",
"speed",
"spell",
"spelt",
"spend",
"spent",
"sperm",
"spice",
"spicy",
"spied",
"spiel",
"spike",
"spiky",
"spill",
"spilt",
"spine",
"spiny",
"spire",
"spite",
"splat",
"split",
"spoil",
"spoke",
"spoof",
"spook",
"spool",
"spoon",
"spore",
"sport",
"spout",
"spray",
"spree",
"sprig",
"spunk",
"spurn",
"spurt",
"squad",
"squat",
"squib",
"stack",
"staff",
"stage",
"staid",
"stain",
"stair",
"stake",
"stale",
"stalk",
"stall",
"stamp",
"stand",
"stank",
"stare",
"stark",
"start",
"stash",
"state",
"stave",
"stead",
"steak",
"steal",
"steam",
"steed",
"steel",
"steep",
"steer",
"stein",
"stern",
"stick",
"stiff",
"still",
"stilt",
"sting",
"stink",
"stint",
"stock",
"stoic",
"stoke",
"stole",
"stomp",
"stone",
"stony",
"stood",
"stool",
"stoop",
"store",
"stork",
"storm",
"story",
"stout",
"stove",
"strap",
"straw",
"stray",
"strip",
"strut",
"stuck",
"study",
"stuff",
"stump",
"stung",
"stunk",
"stunt",
"style",
"suave",
"sugar",
"suing",
"suite",
"sulky",
"sully",
"sumac",
"sunny",
"super",
"surer",
"surge",
"surly",
"sushi",
"swami",
"swamp",
"swarm",
"swash",
"swath",
"swear",
"sweat",
"sweep",
"sweet",
"swell",
"swept",
"swift",
"swill",
"swine",
"swing",
"swirl",
"swish",
"swoon",
"swoop",
"sword",
"swore",
"sworn",
"swung",
"synod",
"syrup",
"tabby",
"table",
"taboo",
"tacit",
"tacky",
"taffy",
"taint",
"taken",
"taker",
"tally",
"talon",
"tamer",
"tango",
"tangy",
"taper",
"tapir",
"tardy",
"tarot",
"taste",
"tasty",
"tatty",
"taunt",
"tawny",
"teach",
"teary",
"tease",
"teddy",
"teeth",
"tempo",
"tenet",
"tenor",
"tense",
"tenth",
"tepee",
"tepid",
"terra",
"terse",
"testy",
"thank",
"theft",
"their",
"theme",
"there",
"these",
"theta",
"thick",
"thief",
"thigh",
"thing",
"think",
"third",
"thong",
"thorn",
"those",
"three",
"threw",
"throb",
"throw",
"thrum",
"thumb",
"thump",
"thyme",
"tiara",
"tibia",
"tidal",
"tiger",
"tight",
"tilde",
"timer",
"timid",
"tipsy",
"titan",
"tithe",
"title",
"toast",
"today",
"toddy",
"token",
"tonal",
"tonga",
"tonic",
"tooth",
"topaz",
"topic",
"torch",
"torso",
"torus",
"total",
"totem",
"touch",
"tough",
"towel",
"tower",
"toxic",
"toxin",
"trace",
"track",
"tract",
"trade",
"trail",
"train",
"trait",
"tramp",
"trash",
"trawl",
"tread",
"treat",
"trend",
"triad",
"trial",
"tribe",
"trice",
"trick",
"tried",
"tripe",
"trite",
"troll",
"troop",
"trope",
"trout",
"trove",
"truce",
"truck",
"truer",
"truly",
"trump",
"trunk",
"truss",
"trust",
"truth",
"tryst",
"tubal",
"tuber",
"tulip",
"tulle",
"tumor",
"tunic",
"turbo",
"tutor",
"twang",
"tweak",
"tweed",
"tweet",
"twice",
"twine",
"twirl",
"twist",
"twixt",
"tying",
"udder",
"ulcer",
"ultra",
"umbra",
"uncle",
"uncut",
"under",
"undid",
"undue",
"unfed",
"unfit",
"unify",
"union",
"unite",
"unity",
"unlit",
"unmet",
"unset",
"untie",
"until",
"unwed",
"unzip",
"upper",
"upset",
"urban",
"urine",
"usage",
"usher",
"using",
"usual",
"usurp",
"utile",
"utter",
"vague",
"valet",
"valid",
"valor",
"value",
"valve",
"vapid",
"vapor",
"vault",
"vaunt",
"vegan",
"venom",
"venue",
"verge",
"verse",
"verso",
"verve",
"vicar",
"video",
"vigil",
"vigor",
"villa",
"vinyl",
"viola",
"viper",
"viral",
"virus",
"visit",
"visor",
"vista",
"vital",
"vivid",
"vixen",
"vocal",
"vodka",
"vogue",
"voice",
"voila",
"vomit",
"voter",
"vouch",
"vowel",
"vying",
"wacky",
"wafer",
"wager",
"wagon",
"waist",
"waive",
"waltz",
"warty",
"waste",
"watch",
"water",
"waver",
"waxen",
"weary",
"weave",
"wedge",
"weedy",
"weigh",
"weird",
"welch",
"welsh",
"whack",
"whale",
"wharf",
"wheat",
"wheel",
"whelp",
"where",
"which",
"whiff",
"while",
"whine",
"whiny",
"whirl",
"whisk",
"white",
"whole",
"whoop",
"whose",
"widen",
"wider",
"widow",
"width",
"wield",
"wight",
"willy",
"wimpy",
"wince",
"winch",
"windy",
"wiser",
"wispy",
"witch",
"witty",
"woken",
"woman",
"women",
"woody",
"wooer",
"wooly",
"woozy",
"wordy",
"world",
"worry",
"worse",
"worst",
"worth",
"would",
"wound",
"woven",
"wrack",
"wrath",
"wreak",
"wreck",
"wrest",
"wring",
"wrist",
"write",
"wrong",
"wrote",
"wrung",
"wryly",
"yacht",
"yearn",
"yeast",
"yield",
"young",
"youth",
"zebra",
"zesty",
"zonal" };
vector<string> SecondGuess;
vector<double> FINALPROBABILITYCOPY;
vector<string> WordListVectorCOPY;
vector<string>WordListVectorFINAL;
vector<double> FINALPROBABILITY;
vector<pair<string, double>> WordListVectorProbability; //this is what needs to be changed
vector<pair<string, double>> WordListVectorProbabilityORIGINAL;
vector<string> CorrectWord;
vector<string> NextGuess;
vector<string> SimilarGuess;
vector<string> ErrorGuess;
vector<int> finalResult;
vector<int> finalResultCOPY;
vector<int> TotalScores;
vector <string> GuessedWords;
unordered_map<string, double> CombinedDATA;
vector<int> removedIndices;


// All variables for Probability Calculations

vector<string> HighestProbabilityWordAfterPlayingTares;


double LetterRemoval2(string NextGuess, vector<string> TempWordList, vector<int> finalResultProbability);
double FinalCalculationsProbability(vector<int> TempProbability);
double log2(double x);


void MakePairOfData2(vector<double> probabilities) {

	WordListVectorProbability.clear();
	for (int i = 0; i < WordListVectorCOPY.size(); i++) {
		WordListVectorProbability.push_back(make_pair(WordListVectorCOPY[i], probabilities[i]));
	}

	/*for (const auto& pair : WordListVectorProbability) {
		cout << pair.first << " -> " << pair.second << endl;  // word -> probability
	}*/

	

}

string Second_Guess(string NextGuess) {

	
	
	string TemporaryGuess;
	
	for (int i = 0; i < WordListVector.size(); i++) {
		
		if (WordListVector[i] == NextGuess) {
			int temp = i;			
			TemporaryGuess = SecondGuess[temp];			
			return TemporaryGuess;
		}
		else {
			continue;
		}
	}

}


void MakePairOfData() {

	WordListVectorProbabilityORIGINAL.clear();
	for (int i = 0; i < WordListVectorFINAL.size(); i++) {
		WordListVectorProbabilityORIGINAL.push_back(make_pair(WordListVectorFINAL[i], FINALPROBABILITY[i]));
	}	
	
}

void ReadInData() {
	ifstream inputFile("12,953_Words.txt");
	string Word;	
	int count = 0;

	if (inputFile.is_open()) {

		do {
			inputFile >> Word;			
			WordListVectorFINAL.push_back(Word);
			
			//cout << Word << endl;
			if (!inputFile.fail()) {
				count++;
			}
		} while (!inputFile.fail());
		inputFile.close();

	}
	else {
		cout << "Could not open file" << endl;
	}

	ifstream inputFile2("12,953_Probabilities.txt");

	double Value;



	if (inputFile2.is_open()) {

		do {

			inputFile2 >> Value;

			FINALPROBABILITY.push_back(Value);
			//cout << Word << endl;
			if (!inputFile2.fail()) {
				count++;
			}


		} while (!inputFile2.fail());
		inputFile2.close();


	}
	else {
		cout << "Could not open file" << endl;
	}



	ifstream inputFile3("SecondGuesses.txt");
	string SecondWord;
	ifstream file("SecondGuesses.txt");
	if (!file.is_open()) {
		cout << "Error: Could not open the file!" << endl;		
	}
	string Doubleword;
	while (file >> Doubleword) {
		SecondGuess.push_back(Doubleword);
	}
	file.close();


	/*for (int i = 0; i < WordListVectorFINAL.size(); i++) {
		cout << WordListVectorFINAL[i] << endl;
	}

	for (int i = 0; i < FINALPROBABILITY.size(); i++) {
		cout << FINALPROBABILITY[i] << endl;
	}
	cout << count << endl;

	system("pause");*/
	
}

string RandomWordGenerator() {
	random_device WordPicker;
	uniform_int_distribution<int> dist(0, 2308);
	int randomNumber = dist(WordPicker);
	string RandomWord = WordListVector[randomNumber];
	CorrectWord.push_back(RandomWord);
	return RandomWord;
}

void Comparison(string CorrectWord, string NextGuess, int placeholder) { // maybe add integers to make it easier to use for all
	if (placeholder == 0) {
		for (int i = 0; i < 5; i++) {
			if (NextGuess[i] == CorrectWord[i]) {
				finalResult.push_back(1);
			}
			else if (NextGuess[i] == CorrectWord[0] || NextGuess[i] == CorrectWord[1] || NextGuess[i] == CorrectWord[2] || NextGuess[i] == CorrectWord[3] || NextGuess[i] == CorrectWord[4]) {
				finalResult.push_back(2);
			}
			else {
				finalResult.push_back(0);
			}
		}
		//	for (int i = 0; i < finalResult.size(); i++) {
		//		cout << finalResult[i];
		//	}
	}

	if (placeholder == 1) {
		for (int i = 0; i < 5; i++) {
			if (NextGuess[i] == CorrectWord[i]) {
				finalResultCOPY.push_back(1);
			}
			else if (NextGuess[i] == CorrectWord[0] || NextGuess[i] == CorrectWord[1] || NextGuess[i] == CorrectWord[2] || NextGuess[i] == CorrectWord[3] || NextGuess[i] == CorrectWord[4]) {
				finalResultCOPY.push_back(2);
			}
			else {
				finalResultCOPY.push_back(0);
			}
		}

	}
}

int CheckForWin() {
	int win = 0;
	if (finalResult[0] == 1 && finalResult[1] == 1 && finalResult[2] == 1 && finalResult[3] == 1 && finalResult[4] == 1) {
		win = 1;
	}
	return win;
}

void LetterRemoval(string NextGuess) {
//	cout << endl;
	//cout << WordListVectorProbability.size() << endl;
	for (int i = 0; i < finalResult.size(); i++) {
		char currentLetter = NextGuess[i];

		if (finalResult[i] == 0) {
			// Remove words that contain the currentLetter anywhere
			WordListVectorCOPY.erase(
				remove_if(WordListVectorCOPY.begin(), WordListVectorCOPY.end(),
					[currentLetter](const string& word) {
						return word.find(currentLetter) != string::npos;
					}),
				WordListVectorCOPY.end());

			WordListVectorProbability.erase(
				remove_if(WordListVectorProbability.begin(), WordListVectorProbability.end(),
					[currentLetter](const pair<string, double>& entry) {
						return entry.first.find(currentLetter) != string::npos;
					}),
				WordListVectorProbability.end());
		}
		else if (finalResult[i] == 2) {
			// Remove words where the currentLetter is in position i
			WordListVectorCOPY.erase(
				remove_if(WordListVectorCOPY.begin(), WordListVectorCOPY.end(),
					[i, currentLetter](const string& word) {
						return word[i] == currentLetter;  // Wrong position check
					}),
				WordListVectorCOPY.end());

			WordListVectorProbability.erase(
				remove_if(WordListVectorProbability.begin(), WordListVectorProbability.end(),
					[i, currentLetter](const pair<string, double>& entry) {
						return entry.first[i] == currentLetter;  // Wrong position check
					}),
				WordListVectorProbability.end());

			// Also remove words that do NOT contain the currentLetter anywhere (i.e., it should be present but not in position i)
			WordListVectorCOPY.erase(
				remove_if(WordListVectorCOPY.begin(), WordListVectorCOPY.end(),
					[currentLetter](const string& word) {
						return word.find(currentLetter) == string::npos;  // Letter must exist somewhere
					}),
				WordListVectorCOPY.end());

			WordListVectorProbability.erase(
				remove_if(WordListVectorProbability.begin(), WordListVectorProbability.end(),
					[currentLetter](const pair<string, double>& entry) {
						return entry.first.find(currentLetter) == string::npos;  // Letter must exist somewhere
					}),
				WordListVectorProbability.end());
		}
		else if (finalResult[i] == 1) {
			// Remove words where currentLetter is NOT in position i
			WordListVectorCOPY.erase(
				remove_if(WordListVectorCOPY.begin(), WordListVectorCOPY.end(),
					[i, currentLetter](const string& word) {
						return word[i] != currentLetter;
					}),
				WordListVectorCOPY.end());

			WordListVectorProbability.erase(
				remove_if(WordListVectorProbability.begin(), WordListVectorProbability.end(),
					[i, currentLetter](const pair<string, double>& entry) {
						return entry.first[i] != currentLetter;
					}),
				WordListVectorProbability.end());
		}
	}
//	cout << endl;
//	cout << WordListVectorProbability.size() << endl;
	
	
}

string NextGuessProbability() {
	auto maxIt = max_element(WordListVectorProbability.begin(), WordListVectorProbability.end(),
		[](const pair<string, double>& a, const pair<string, double>& b) {
			return a.second < b.second;
		});
	string NextGuess = maxIt->first;
	//cout << endl;
	//cout << "This is the next guess " << NextGuess << endl;
	
	return NextGuess;
}

void StoringValues() {
	auto maxIt = max_element(WordListVectorProbability.begin(), WordListVectorProbability.end(),
		[](const pair<string, double>& a, const pair<string, double>& b) {
			return a.second < b.second;
		});
	string NextGuess = maxIt->first;
	//cout << endl;
	//cout << "This is the next guess " << NextGuess << endl;
	HighestProbabilityWordAfterPlayingTares.push_back(NextGuess);
	cout << "Value stored in vector " << endl;
	for (int i = 0; i < HighestProbabilityWordAfterPlayingTares.size(); i++) {
		cout << HighestProbabilityWordAfterPlayingTares[i] << endl;
	}
}


void ProbabilityCalculations() {
	//generate every combination
	vector<vector<int>> combinations;
	vector<string> TempWordList;
	vector<double> Temp2Probability;
	double TempProbability;
	vector<double> EndProbability;// this is the number we need
	double Temp = 0.0;
	for (int i = 0; i < 3; ++i) {
		for (int j = 0; j < 3; ++j) {
			for (int k = 0; k < 3; ++k) {
				for (int l = 0; l < 3; ++l) {
					for (int m = 0; m < 3; ++m) {
						vector<int> currentCombination = { i, j, k, l, m };
						combinations.push_back(currentCombination);
					}
				}
			}
		}
	}

	/*for (const auto& combination : combinations) {
		for (int num : combination) {
			cout << num << " ";
		}
		cout << endl; // Move to the next line after printing each combination
	}
	system("pause");*/

	for (int i = 0; i < WordListVectorCOPY.size(); i++){
		TempWordList = WordListVectorCOPY;
		string WordToCalculate = TempWordList[i];
		//cout << WordToCalculate << " " << i << endl;

		for (size_t k = 0; k < combinations.size(); k++) {
			vector<int> FINALResult = combinations[k];
			TempWordList = WordListVectorCOPY;
			TempProbability = LetterRemoval2(WordToCalculate, TempWordList, FINALResult); //define
			//cout << TempProbability << endl;
			Temp2Probability.push_back(TempProbability);
			//system("pause");
		}
		double sum = 0.0;
		for (int l = 0; l < Temp2Probability.size(); l++){
			sum += Temp2Probability[l];
		}		
		EndProbability.push_back(sum);
		Temp2Probability.clear();
	}
	
	MakePairOfData2(EndProbability);
	EndProbability.clear();

	/*cout << WordListVectorCOPY.size() << endl;
	cout << EndProbability.size() << endl;
	for (int j = 0; j < EndProbability.size(); j++) {
		cout << EndProbability[j] << endl;
	}
	system("pause");*/
}

double LetterRemoval2(string NextGuess, vector<string> TempWordList, vector<int> finalResultProbability) { //where you last left off
	vector<string> ProbabilityWordList = TempWordList;
	vector<int> ProbabilityFinalResult = finalResultProbability;
	double TempProbability = 0;
	
	
	double initialWordCount = ProbabilityWordList.size();
	//cout << initialWordCount << endl;

	for (int i = 0; i < ProbabilityFinalResult.size(); i++) {
		if (ProbabilityFinalResult[i] == 0) {
			char letterToRemove = NextGuess[i];
			ProbabilityWordList.erase(
				remove_if(ProbabilityWordList.begin(), ProbabilityWordList.end(),
					[letterToRemove](const string& word) {
						return word.find(letterToRemove) != string::npos;
					}),
				ProbabilityWordList.end());
		}

		if (ProbabilityFinalResult[i] == 2) {
			char temporaryLetter = NextGuess[i];
			ProbabilityWordList.erase(
				remove_if(ProbabilityWordList.begin(), ProbabilityWordList.end(),
					[i, temporaryLetter](const string& word) {
						return word[i] == temporaryLetter;
					}),
				ProbabilityWordList.end());
		}

		if (ProbabilityFinalResult[i] == 1) {
			char correctLetter = NextGuess[i];
			ProbabilityWordList.erase(
				remove_if(ProbabilityWordList.begin(), ProbabilityWordList.end(),
					[i, correctLetter](const string& word) {
						return word[i] != correctLetter;
					}),
				ProbabilityWordList.end());
		}

		if (ProbabilityFinalResult[i] == 2) {
			char correctLetter = NextGuess[i];
			ProbabilityWordList.erase(
				remove_if(ProbabilityWordList.begin(), ProbabilityWordList.end(),
					[correctLetter](const string& word) {
						return word.find(correctLetter) == string::npos;
					}),
				ProbabilityWordList.end());
		}
	}
	double remainingWordCount = ProbabilityWordList.size();
	if (remainingWordCount == 0) {
		return 0.0;
	}
	//cout << remainingWordCount << endl;
	double Probability = remainingWordCount / initialWordCount;
	//cout << Probability << endl;
	double LogFactor = log2(1 / Probability);
	//cout << LogFactor << endl;
	TempProbability = Probability * LogFactor;
	//cout << TempProbability << endl;
	//system("pause");
//	if (isnan(TempProbability)) {
	//	return 0.0;  // Return 0 if TempProbability is NaN
//	}
	
	return TempProbability;
	
}

double log2(double x) {
	return log(x) / log(2);
}

double  FinalCalculationsProbability(vector<int> TempProbability) {
	double sum = 0.0;
	vector<int> Crazy = TempProbability;
	cout << "Here we are " << endl;
	for (int i = 0; i < Crazy.size(); i++) {
		cout << Crazy[i] << endl;
	}
	cout << "Here we are " << endl;
	cout << Crazy.size() << endl;
	system("pause");
	for (double prob : Crazy) {
		if (!isnan(prob)) {
			
			sum += prob;
		}
	}
	cout << "Sum of TempProbability: " << sum << endl;
	
	return sum;
}





int main() {

	auto start = high_resolution_clock::now();
	ReadInData();
	MakePairOfData();
	
	for (int i = 0; i < WordListVector.size(); i++){
	//for (int i = 0; i < 1000; i++) {
		//system("pause");
		WordListVectorProbability.clear();
		WordListVectorProbability = WordListVectorProbabilityORIGINAL;
		WordListVectorCOPY = WordListVectorFINAL;




		int totalGuesses = 0;

		string CorrectWord = WordListVector[i]; //which is not doing it right it is not getting rid of the c in the first position
		//string CorrectWord = RandomWordGenerator();
		//cout << CorrectWord << endl;
		string NextGuess = "tares";
		//string NextGuess = NextGuessProbability();
		//cout << NextGuess << endl;
		//string NextGuess = RandomWordGenerator();
		GuessedWords.push_back(NextGuess);
		//	WordListVectorCOPY.erase(remove(WordListVectorCOPY.begin(), WordListVectorCOPY.end(), NextGuess), WordListVectorCOPY.end());	

		Comparison(CorrectWord, NextGuess, 0);
		int CheckingForWin = CheckForWin();
		if (CheckingForWin == 1) {
			cout << endl;
			cout << "This is the total number of Guesses " << totalGuesses << "     ";
			cout << "{";
			for (int k = 0; k < GuessedWords.size(); k++) {
				cout << GuessedWords[k] << " ";
			}
			cout << "}                                                                               ";
			cout << i;

			totalGuesses++;
			TotalScores.push_back(totalGuesses);
			GuessedWords.clear();
			finalResult.clear();
			continue;
		}

		LetterRemoval(NextGuess);
		totalGuesses++;
		int PLACEHOLDER = 1;
		
		while (PLACEHOLDER != 0) {
			
			//StoringValues(WordListVector[i]);
			if (totalGuesses == 1) {
				NextGuess = Second_Guess(CorrectWord);
			}
			
			else {
				ProbabilityCalculations();
				NextGuess = NextGuessProbability();
			}
			
			
			GuessedWords.push_back(NextGuess);
			finalResult.clear();
			totalGuesses++;
			Comparison(CorrectWord, NextGuess, 0);
			int PlaceHolder = CheckForWin();
			if (PlaceHolder == 1) {
				PLACEHOLDER = 0;

			}
			//	WordListVectorCOPY.erase(remove(WordListVectorCOPY.begin(), WordListVectorCOPY.end(), NextGuess), WordListVectorCOPY.end());
			LetterRemoval(NextGuess);
			if (PlaceHolder == 1) {
				PLACEHOLDER = 0;


			}
			finalResult.clear();
		}

		cout << endl;
		cout << "This is the total number of Guesses " << totalGuesses << "     ";
		cout << "{";
		for (int k = 0; k < GuessedWords.size(); k++) {
			cout << GuessedWords[k] << " ";
		}
		cout << "}   " << CorrectWord << "                                                                              ";
		cout << i;
		TotalScores.push_back(totalGuesses);
		GuessedWords.clear();
		//system("pause");
	}

	

	int sum = 0;
	for (int score : TotalScores) {
		sum += score;
	}

	double average = static_cast<double>(sum) / TotalScores.size();

	int minScore = *min_element(TotalScores.begin(), TotalScores.end());
	int maxScore = *max_element(TotalScores.begin(), TotalScores.end());

	int countBelowOrEqualToSix = count_if(TotalScores.begin(), TotalScores.end(), [](int score) {
		return score <= 6;
		});
	double percentageBelowOrEqualToFive = (static_cast<double>(countBelowOrEqualToSix) / TotalScores.size()) * 100;

	for (int j = 0; j < 10; j++) {
		cout << endl;
	}

	cout << "The average score of the Wordle Bot of 6,000,000 games is" << endl;
	cout << average;
	cout << endl;
	cout << "Lowest number: " << minScore << endl;
	cout << "Highest number: " << maxScore << endl;
	cout << "Percentage of Wins " << percentageBelowOrEqualToFive << "%" << endl;

	unordered_map<int, int> frequency;
	for (int score : TotalScores) {
		frequency[score]++;
	}

	cout << "Frequency of each number:" << endl;
	for (int i = minScore; i <= maxScore; ++i) {
		cout << i << ": " << frequency[i] << endl;
	}

	auto stop = high_resolution_clock::now();
	auto durations = duration_cast<duration<double>>(stop - start); // Duration in seconds with decimals

	cout << "Time taken: " << durations.count() << " seconds" << endl;
	return 0;
}