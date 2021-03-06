    let k = ./k_types.dhall 

in  let defaultService =
          { kind = k.Kinds.Service {=}, apiVersion = k.ApiVersions.v1 {=} }

in  let tcp = { protocol = k.Protocols.TCP {=} }

in  k.makeYaml
    (   defaultService
      ∧ { metadata =
            { name = "my-service-1" }
        , spec =
            { selector =
                { app = "MyApp1" }
            , ports =
                [ tcp ∧ { port = 80, targetPort = 9376 } ]
            }
        }
    )
