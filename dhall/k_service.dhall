    let k = ./k_types.dhall 

in  k.makeYaml
    { kind =
        k.Kinds.Service {=}
    , apiVersion =
        k.ApiVersions.v1 {=}
    , metadata =
        { name = "my-service" }
    , spec =
        { selector =
            { app = "MyApp" }
        , ports =
            [ { protocol = k.Protocols.TCP {=}, port = 80, targetPort = 9376 } ]
        }
    }
