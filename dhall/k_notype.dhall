{ kind =
    "Service"
, apiVersion =
    "v1"
, metadata =
    { name = "my-service" }
, spec =
    { selector =
        { app = "MyApp" }
    , ports =
        [ { protocol = "TCP", port = 80, targetPort = 9376 } ]
    }
}
