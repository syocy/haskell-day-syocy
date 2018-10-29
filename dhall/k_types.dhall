    let Kind_ = < Service : {} | Pod : {} | Deployment : {} >

in  let ApiVersion = < v1 : {} >

in  let Metadata = { name : Text }

in  let Selector = { app : Text }

in  let Protocol = < TCP : {} | UDP : {} >

in  let Port = { protocol : Protocol, port : Natural, targetPort : Natural }

in  let Spec = { selector : Selector, ports : List Port }

in  let Prelude/List/map =
          https://raw.githubusercontent.com/dhall-lang/Prelude/35deff0d41f2bf86c42089c6ca16665537f54d75/List/map 

in  let makeYaml =
            λ ( args
              : { kind :
                    Kind_
                , apiVersion :
                    ApiVersion
                , metadata :
                    Metadata
                , spec :
                    Spec
                }
              )
          → { kind =
                merge
                { Service =
                    λ(_ : {}) → "Service"
                , Pod =
                    λ(_ : {}) → "Pod"
                , Deployment =
                    λ(_ : {}) → "Deployment"
                }
                args.kind
            , apiVersion =
                merge { v1 = λ(_ : {}) → "v1" } args.apiVersion
            , metadata =
                args.metadata
            , spec =
                { selector =
                    args.spec.selector
                , ports =
                    Prelude/List/map
                    Port
                    { protocol : Text, port : Natural, targetPort : Natural }
                    (   λ(port : Port)
                      →   port
                        ⫽ { protocol =
                              merge
                              { TCP =
                                  λ(_ : {}) → "TCP"
                              , UDP =
                                  λ(_ : {}) → "UDP"
                              }
                              port.protocol
                          }
                    )
                    args.spec.ports
                }
            }

in  { Kinds =
        constructors Kind_
    , ApiVersions =
        constructors ApiVersion
    , Protocols =
        constructors Protocol
    , emptyPorts =
        [] : List Port
    , makeYaml =
        makeYaml
    }
