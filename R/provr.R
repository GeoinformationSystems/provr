

pkg.env <- new.env()

is.url <- function(x) {
    grepl("www.|http:|https:", x)
}


init_provenance_graph <- function(namespace = NULL, file = NULL) {
    if (!is.url(namespace)) {
        stop(
            "Please provide a valid namespace for the objects of your provenance graph.
        - E.g. http://yourscriptname.org#.
        - The namespace has to be formatted like a URI, but does not have to be an actual URL on the web."
        )
    }

    # either load existing graph or init new one
    if (!(is.null(file))) {
        pkg.env$rdf <- rdflib::rdf_parse(file)
    } else {
        pkg.env$rdf <- rdflib::rdf()
    }

    last_namespace_char <- substring(namespace, nchar(namespace))
    if (last_namespace_char != "#" || last_namespace_char != "/") {
        namespace <<- paste0(namespace, "#")
    }

    pkg.env$namespaces <- c(
        rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
        rdfs = "http://www.w3.org/2000/01/rdf-schema#",
        prov = "http://www.w3.org/ns/prov#",
        rscript = namespace
    )
}

serialize_provenance_graph <- function(name, format = "turtle") {
    rdflib::rdf_serialize(pkg.env$rdf, name, namespace = pkg.env$namespaces, format = format)
}

resource_exists <- function(resource_id, namespace = pkg.env$namespaces["rscript"]) {

    # check if resource exists as a subject (-> if yes, it was defined previously)
    query <- sprintf("SELECT ?p ?o WHERE {
        <%s> ?p ?o .
    }", paste0(namespace, URLencode(resource_id, reserved = TRUE)))
    # suppress warning if query has no solution (which is more or less the default case)
    suppressWarnings(length <- nrow(rdflib::rdf_query(pkg.env$rdf, query)))

    return(as.logical(length))
}



Entity <- function(id = NULL, label = NULL, description = NULL, namespace = pkg.env$namespaces["rscript"], load = FALSE) {
    if (is.null(id)) {
        stop("ID required!")
    }
    id_exists <- resource_exists(id, namespace)
    if (id_exists && !(load)) {
        stop(sprintf("A resource with the URI <%s> already exists (as subject), please use the \'load = TRUE\' option.", paste0(namespace, URLencode(id, reserved = TRUE))))
    }
    if (load && !(id_exists)) {
        stop(sprintf("A resource with the URI <%s> does not exists (as subject).", paste0(namespace, URLencode(id, reserved = TRUE))))
    }

    id <- id
    label <- label
    description <- description

    if (!(load)) {
        pkg.env$rdf %>% rdflib::rdf_add(
            subject = paste0(namespace, URLencode(id, reserved = TRUE)),
            predicate = paste0(pkg.env$namespaces["rdf"], "type"),
            object = paste0(pkg.env$namespaces["prov"], "Entity")
        )
        if (is.null(label)) {
            pkg.env$rdf %>% rdflib::rdf_add(
                subject = paste0(namespace, URLencode(id, reserved = TRUE)),
                predicate = paste0(pkg.env$namespaces["rdfs"], "label"),
                object = id
            )
        } else {
            pkg.env$rdf %>% rdflib::rdf_add(
                subject = paste0(namespace, URLencode(id, reserved = TRUE)),
                predicate = paste0(pkg.env$namespaces["rdfs"], "label"),
                object = label
            )
        }

        if (!is.null(description)) {
            pkg.env$rdf %>% rdflib::rdf_add(
                subject = paste0(namespace, URLencode(id, reserved = TRUE)),
                predicate = paste0(pkg.env$namespaces["rdfs"], "comment"),
                object = toString(description)
            )
        }
    }


    get_id <- function() id
    get_label <- function() label
    get_description <- function() description

    wasGeneratedBy <- function(activity) {
        if (class(activity) != "Activity") {
            stop("argument has to be of the class \"Activity\"!")
        }
        pkg.env$rdf %>% rdflib::rdf_add(
            subject = paste0(namespace, URLencode(id, reserved = TRUE)),
            predicate = paste0(pkg.env$namespaces["prov"], "wasGeneratedBy"),
            object = paste0(namespace, URLencode(activity$get_id(), reserved = TRUE)),
        )
    }

    wasDerivedFrom <- function(entity) {
        if (toString(class(entity)) != "Entity") {
            stop("argument has to be of the class \"Entity\"!")
        }
        pkg.env$rdf %>% rdflib::rdf_add(
            subject = paste0(namespace, URLencode(id, reserved = TRUE)),
            predicate = paste0(pkg.env$namespaces["prov"], "wasDerivedFrom"),
            object = paste0(namespace, URLencode(entity$get_id(), reserved = TRUE)),
        )
    }

    wasAttributedTo <- function(agent) {
        if (toString(class(agent)) != "Agent") {
            stop("argument has to be of the class \"Agent\"!")
        }
        pkg.env$rdf %>% rdflib::rdf_add(
            subject = paste0(namespace, URLencode(id, reserved = TRUE)),
            predicate = paste0(pkg.env$namespaces["prov"], "wasAttributedTo"),
            object = paste0(namespace, URLencode(entity$get_id(), reserved = TRUE)),
        )
    }
    structure(class = "Entity", environment())
}

Activity <- function(id = NULL, label = NULL, description = NULL, namespace = pkg.env$namespaces["rscript"], load = FALSE) {
    if (is.null(id)) {
        stop("ID required!")
    }
    id_exists <- resource_exists(id, namespace)
    if (id_exists && !(load)) {
        stop(sprintf("A resource with the URI <%s> already exists (as subject), please use the \'load = TRUE\' option.", paste0(namespace, URLencode(id, reserved = TRUE))))
    }
    if (load && !(id_exists)) {
        stop(sprintf("A resource with the URI <%s> does not exists (as subject).", paste0(namespace, URLencode(id, reserved = TRUE))))
    }
    id <- id
    label <- label
    description <- description

    if (!(load)) {
        pkg.env$rdf %>% rdflib::rdf_add(
            subject = paste0(namespace, URLencode(id, reserved = TRUE)),
            predicate = paste0(pkg.env$namespaces["rdf"], "type"),
            object = paste0(pkg.env$namespaces["prov"], "Activity")
        )
        if (is.null(label)) {
            pkg.env$rdf %>% rdflib::rdf_add(
                subject = paste0(namespace, URLencode(id, reserved = TRUE)),
                predicate = paste0(pkg.env$namespaces["rdfs"], "label"),
                object = id
            )
        } else {
            pkg.env$rdf %>% rdflib::rdf_add(
                subject = paste0(namespace, URLencode(id, reserved = TRUE)),
                predicate = paste0(pkg.env$namespaces["rdfs"], "label"),
                object = label
            )
        }

        if (!is.null(description)) {
            pkg.env$rdf %>% rdflib::rdf_add(
                subject = paste0(namespace, URLencode(id, reserved = TRUE)),
                predicate = paste0(pkg.env$namespaces["rdfs"], "comment"),
                object = toString(description)
            )
        }
    }

    get_id <- function() id
    get_label <- function() label
    get_description <- function() description

    used <- function(entity) {
        if (toString(class(entity)) != "Entity") {
            stop("argument has to be of the class \"Entity\"!")
        }
        pkg.env$rdf %>% rdflib::rdf_add(
            subject = paste0(namespace, URLencode(id, reserved = TRUE)),
            predicate = paste0(pkg.env$namespaces["prov"], "used"),
            object = paste0(namespace, URLencode(entity$get_id(), reserved = TRUE)),
        )
    }

    wasInformedBy <- function(activity) {
        if (class(activity) != "Activity") {
            stop("argument has to be of the class \"Activity\"!")
        }
        pkg.env$rdf %>% rdflib::rdf_add(
            subject = paste0(namespace, URLencode(id, reserved = TRUE)),
            predicate = paste0(pkg.env$namespaces["prov"], "wasInformedBy"),
            object = paste0(namespace, URLencode(activity$get_id(), reserved = TRUE)),
        )
    }

    wasAssociatedWith <- function(agent) {
        if (toString(class(agent)) != "Agent") {
            stop("argument has to be of the class \"Agent\"!")
        }
        pkg.env$rdf %>% rdflib::rdf_add(
            subject = paste0(namespace, URLencode(id, reserved = TRUE)),
            predicate = paste0(pkg.env$namespaces["prov"], "wasAssociatedWith"),
            object = paste0(namespace, URLencode(entity$get_id(), reserved = TRUE)),
        )
    }
    structure(class = "Activity", environment())
}


Agent <- function(id = NULL, label = NULL, description = NULL, namespace = pkg.env$namespaces["rscript"], load = FALSE) {
    if (is.null(id)) {
        stop("ID required!")
    }
    id_exists <- resource_exists(id, namespace)
    if (id_exists && !(load)) {
        stop(sprintf("A resource with the URI <%s> already exists (as subject), please use the \'load = TRUE\' option.", paste0(namespace, URLencode(id, reserved = TRUE))))
    }
    if (load && !(id_exists)) {
        stop(sprintf("A resource with the URI <%s> does not exists (as subject).", paste0(namespace, URLencode(id, reserved = TRUE))))
    }
    id <- id
    label <- label
    description <- description

    if (!(load)) {
        pkg.env$rdf %>% rdflib::rdf_add(
            subject = paste0(namespace, URLencode(id, reserved = TRUE)),
            predicate = paste0(pkg.env$namespaces["rdf"], "type"),
            object = paste0(pkg.env$namespaces["prov"], "Agent")
        )
        if (is.null(label)) {
            pkg.env$rdf %>% rdflib::rdf_add(
                subject = paste0(namespace, URLencode(id, reserved = TRUE)),
                predicate = paste0(pkg.env$namespaces["rdfs"], "label"),
                object = id
            )
        } else {
            pkg.env$rdf %>% rdflib::rdf_add(
                subject = paste0(namespace, URLencode(id, reserved = TRUE)),
                predicate = paste0(pkg.env$namespaces["rdfs"], "label"),
                object = label
            )
        }

        if (!is.null(description)) {
            pkg.env$rdf %>% rdflib::rdf_add(
                subject = paste0(namespace, URLencode(id, reserved = TRUE)),
                predicate = paste0(pkg.env$namespaces["rdfs"], "comment"),
                object = toString(description)
            )
        }
    }

    get_id <- function() id
    get_label <- function() label
    get_description <- function() description
    actedOnBehalfOf <- function(agent) {
        if (toString(class(entity)) != "Agent") {
            stop("argument has to be of the class \"Agent\"!")
        }
        pkg.env$rdf %>% rdflib::rdf_add(
            subject = paste0(namespace, URLencode(id, reserved = TRUE)),
            predicate = paste0(pkg.env$namespaces["prov"], "actedOnBehalfOf"),
            object = paste0(namespace, URLencode(entity$get_id(), reserved = TRUE)),
        )
    }
    structure(class = "Agent", environment())
}