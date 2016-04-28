aux <- new.env(parent = .GlobalEnv)

pkg_loader(c("xlsx", "gridExtra", "RODBC", "ReporteRs", "RColorBrewer"))


aux$span_dt <- date_expansion(start = input$start_dt, end = input$end_dt,
                              type = c("qtr", "fy"))
aux$shortVendor <- function(x) {recode_string(x, recode_key =
  list(
    CHS = c("CHS Group LLC.", "CHS"),
    JOAK = c("JOAK", "JOAK American Homes"),
    INI = c("INI", "INI GROUP LLC (Formerly Micholdings)", "INI GROUP LLC"),
    Synod = c("Synod Community Services", "Synod",
              "Synod Residential Services"),
    PPA = c("Partners In Personal Assistance", "PPA"),
    PRS = c("Progressive Residential Services, Inc.", "PRS"),
    Renassance = c("Renaissance House, Inc.", "Renaissance Community Homes Inc",
                   "Renaissance"),
    CSS = c("Catholic Social Services of Washtenaw Co", "CSS"),
    Quest = c("QUEST INC", "Quest"),
    ALS = c("Adult Learning Systems- Master", "ALS"),
    CSI = c("CONSUMER SERVICES, INC", "CSI"),
    Real = c("Real Life Living Services, Inc.", "Real"),
    HEIOTS = c("His Eye is on the Sparrow", "HEIOTS",
               'His Eye is on the Sparrow- Marblewood'),
    Macomb = c("Macomb Residential Opportunities - Master", "Macomb"),
    CRC = c("Community Residence Corp.", "CRC"),
    Spectrum = c("Spectrum Community Services", "Spectrum"),
    `T.Leaf` = c("TURNING LEAF REHABILITATION SERVICES, INC", "T.Leaf"),
    `Wash CMH` = c("Wash CMH", "Washtenaw County Community Mental Health",
                   "Washtenaw County Community Mental Health-External"),
    `Comp Svc: DD` =
      c("Comprehensive Services for the Developmentally Disabled",
        "Comp Svc: DD")))
}

aux$colors <- RColorBrewer::brewer.pal(4, "Blues")


# search function
aux$mySearch <- function(x, pattern, ignore.case = TRUE, value=TRUE) {
  pattern = paste0(pattern, sep="|", collapse="")
  pattern = substr(x=pattern, 1, (nchar(pattern)-1))
  result = grep(x = x, pattern = pattern, ignore.case = ignore.case, value=value)
  return(result)
}
 # example
 # mySearch(x=allMeds, pattern=benzoList)

# list of benzos - http://en.wikipedia.org/wiki/List_of_benzodiazepines
aux$benzoList = c("Alprazolam", "Helex", "Xanax", "Xanor", "Onax", "Alprox", "Restyl", "Tafil", "Paxal",
                "Bretazenil",
                "Bromazepam", "Lectopam", "Lexotanil", "Lexotan", "Bromam",
                "Brotizolam", "Lendormin", "Dormex", "Sintonal", "Noctilan",
                "Chlordiazepoxide", "Librium", "Risolid", "Elenium",
                "Cinolazepam",  "Gerodorm",
                "Clonazepam", "Rivatril", "Rivotril", "Klonopin", "Iktorivil", "Paxam",
                "Clorazepate", "Tranxene", "Tranxilium",
                "Clotiazepam", "Veratran", "Clozan", "Rize",
                "Cloxazolam", "Sepazon", "Olcadil",
                "Delorazepam", "Dadumir",
                "Diazepam", "Antenex", "Apaurin", "Apzepam", "Apozepam", "Hexalid", "^Pax$",
                  "Stesolid", "Stedon", "Valium", "Vival", "Valaxona",
                "Estazolam", "ProSom",
                "Etizolam", "Etilaam", "Etizest", "Pasaden", "Depas",
                "Ethyl loflazepate", "Victan", "Meilax", "Ronlax",
                "Flunitrazepam", "Rohypnol", "Hipnosedon", "Vulbegal", "Fluscand", "Flunipam", "Ronal", "Rohydorm",
                "Flurazepam", "Dalmadorm", "Dalmane",
                "Flutoprazepam", "^Restas$",
                "Halazepam", "Paxipam",
                "Ketazolam", "Anxon",
                "Loprazolam", "Dormonoct",
                "Lorazepam", "Ativan", "Lorenin", "Lorsilan", "Temesta", "Tavor", "Lorabenz",
                "Lormetazepam", "Loramet", "Noctamid", "Pronoctan",
                "Medazepam", "Nobrium",
                "Midazolam", "Dormicum", "Versed", "Hypnovel", "Dormonid",
                "Nimetazepam", "Erimin",
                "Nitrazepam", "Mogadon", "Alodorm", "Pacisyn", "Dumolid", "Nitrazadon",
                "Nordazepam", "Madar", "Stilny",
                "Onfi", "clobazam", "Frisium", "Urbanol",
                "Oxazepam", "Seresta", "Serax", "Serenid", "Serepax", "Sobril", "Oxabenz", "Oxapax", "Opamox",
                "Phenazepam", "Phenazepam",
                "Pinazepam", "Domar",
                "Prazepam", "Lysanxia", "Centrax",
                "Premazepam",
                "Pyrazolam",
                "Quazepam", "Doral",
                "Temazepam", "Restoril", "Normison", "Euhypnos", "Temaze", "Tenox",
                "Tetrazepam", "Myolastan",
                "Triazolam", "Halcion", "Rilamir",
                "Camazepam", "Carburazepam", "Cinazepam",
                "Cyprazepam", "Demoxepam", "Devazepide",
                "Diclazepam", "Doxefazepam", "Elfazepam", "Ethyl carfluzepate", "Ethyl dirazepate",
                "Flubromazepam", "Fletazepam", "Fludiazepam", "Flutemazepam",
                "Fosazepam", "Gidazepam", "Iclazepam", "Lufuradom",
                "Meclonazepam", "Menitrazepam", "Metaclazepam", "Motrazepam", "Nitrazepate",
                "Nortetrazepam", "Pivoxazepam", "Proflazepam",
                "QH-II-66", "Reclazepam", "RO4491533", "Ro5-2904", "Ro5-4864", "Sulazepam",
                "Tifluadom", "Tolufazepam", "Tuclazepam", "Uldazepam", "Arfendazam",
                "CP-1414S", "Lofendazam",
                "Triflubazam", "Girisopam", "GYKI-52466", "GYKI-52895", "Nerisopam", "Talampanel",
                  "Tofisopam", "Adinazolam",
                "Flubromazolam",
                "Climazolam", "FG-8205", "Imidazenil", "Iomazenil", "L-655,708",
                "PWZ-029", "Remimazolam", "Ro15-4513", "Ro48-6791", "Ro48-8684", "Ro4938581", "Sarmazenil",
                "SH-053-R-CH3-'F", "Flutazolam", "Haloxazolam", "Mexazolam", "Oxazolam",
                "Bentazepam", "Ciclotizolam", "JQ1",
                "Lopirazepam", "Zapizolam",
                "Razobazam", "Ripazepam", "Zolazepam", "Zomebazam", "Zometapine",
                "Clazolam", "Avizafone", "Rilmazafone"
                )
                # pax should not get paxal
                # restas should not get Restasis
                # selenium should not show up

##### Atypical benzodiazepine receptor ligands #####
  # "Zolpidem", "Ambien", "Nytamel", "Sanval", "Stilnoct", "Stilnox", "Sublinox (Canada)",
  # "Xolnox", "Zoldem", "Zolnod",
  # "DMCM",
  # "Flumazenil", "Anexate", "Lanexat", "Mazicon", "Romazicon",
  # "Eszopiclone", "Lunesta",
  # "Zaleplon", "Sonata", "Starnoc",
  # "Zopiclone", "Imovane", "Rhovane", "Ximovan", "Zileze", "Zimoclone", "Zimovane", "Zopitan", "Zorclone",


# list of stimulants
aux$stimList = c("adderall", "concerta", "daytrana", "dexedrine", "focalin", "Didrex", "benzphetamine",
               "metadate", "methylphenidate", "provigil", "ritalin", "vyvanse", "lisdexamfetamine dimesylate",
               "armodafinil", "Nuvigil")

# list of atypical antipsychotics
aux$atypicalList = data.table( class=c("AAP1", rep("AAP2", 2), "AAP3", rep("AAP4", 2),
                                     rep("AAP5", 3), rep("AAP6", 2), rep("AAP7", 2),
                                     rep("AAP8", 3)),
                             meds = c("abilify", # aap1
                                      "invega", "risper...", # aap2
                                      "saphris", # app3
                                      "quetiapine", "seroquel", # app4
                                      "invega sustenna", "fanapt", "iloperidone", # app5
                                      "geodon", "ziprasidone hcl", # app6
                                      "zyprexa", "olanzapine", # app7
                                      "clozaril", "clozapine", "fazaclo")) # app8

aux$aTypical = function(data) {
    aap1 = grep(x=data, pattern="abilify", ignore.case=TRUE, value=TRUE)
    aap2 = grep(x=data, pattern="invega|risper", ignore.case=TRUE, value=TRUE)
    aap2 = grep(x=aap2, pattern="^invega sustenna", ignore.case=TRUE, value=TRUE, invert=TRUE)
    aap3 = grep(x=data, pattern="saphris", ignore.case=TRUE, value=TRUE)
    aap4 = grep(x=data, pattern="quetiapine|seroquel", ignore.case=TRUE, value=TRUE)
    aap5 = grep(x=data, pattern="invega sustenna|fanapt|iloperidone", ignore.case=TRUE, value=TRUE)
    aap6 = grep(x=data, pattern="geodon|ziprasidone hcl", ignore.case=TRUE, value=TRUE)
    aap7 = grep(x=data, pattern="zyprexa|olanzapine", ignore.case=TRUE, value=TRUE)
    aap8 = grep(x=data, pattern="clozaril|clozapine|fazaclo", ignore.case=TRUE, value=TRUE)

  AAPclass=NULL
  aapList=NULL
  for( i in 1:8) {
    AAPclass = c(AAPclass, rep(paste0("AAP",i), length( get(paste0("aap",i) ))))
    aapList = c(aapList, get(paste0("aap", i)))
  }
  aapDT = data.table(AAPclass=AAPclass, drug=aapList)
  return(aapDT)
  }

# function to count length without NA
aux$length_noNA = function(x) {
    x = x[!is.na(x)]
    result = length(x)
    return(result)
  }

# list of antipsychotics
aux$antiPysList = c("thorazine", "chlopromazine", "stelazine", "trifluoperazine",
                "serentil", "mesoridazine", "thioridazine", "mellaril",
                "thiothixene", "navane", "perphenazine", "trilafon",
                "chlorprothixene", "taractan", "pimozide", "orap",
                "moban", "molindone", "loxapine", "loxitane", "zyprexa",
                "latuda", "lurasidone", "fanapt", "iloperidone",
                "olanzapine", "seroquel", "quetiapine", "ziprasidone", "geodon",
                "clozapine", "clozaril", "aripiprazole", "abilify", "prolixin",
                "fluphenazine", "haldol", "haloperidol", "risper", "consta",
                "invega", "paliperidone", "palperidone", "abilify maintena", "saphris", "asenapine")

# list of antidepressants
aux$antidepressList = c("maprotiline", "ludiomil", "mirtazapine", "mirtazapine",
                    "remeron", "nefazodon", "nefazodone", "serzone", "trazodon", "trazodone", "desyrel",
                    "wellbutrin", "buproprion", "effexor", "venlafaxine", "marplan", "isocarboxazid",
                    "nardil", "phenelzine", "tranylcypromine", "parnate", "zoloft",
                    "sertraline", "prozac", "fluoxetine", "paroxetine", "paxil", "paroxetine",
                    "fluvoxamine", "luvox", "citalopram", "celexa", "escitalopram",
                    "lexapro", "duloxetine", "cymbalta", "nortriptyline", "pamelor",
                    "desipramine", "norpramin", "protriptyline", "vivactil", "amoxapine",
                    "ascendin", "amitriptyline", "elavil", "imipramine", "tofranil",
                    "doxepin", "sinequan", "dlomipramine", "anafranil", "trimipramine",
                    "surmontil", "atomoxetine", "strattera",
                    "emsam", "selegiline transdermal", "brentillix", "vortioxetine", "forfivo XL",
                    "bupropion hydrochloride")