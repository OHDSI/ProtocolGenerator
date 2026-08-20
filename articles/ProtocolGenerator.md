# Using ProtocolGenerator

To run protocol generator you need to have a `Strategus` json
specification (either as a saved file or loaded into R). You can specify
a `webAPI` to use for concept set extraction (note: rendered concept
sets can change depending on the webAPI’s vocab). The `outputLocation`
is where the protocol html file will be saved to.

You can personalize the protocol with the following inputs:

- protocolSubheading: this lets you specify a title for the study
- headerColor: this lets you specify a header color for the protocol
  (defaults to dark blue)
- headerLogoLocation: this lets you specify a logo to add to the
  protocol header (defaults to no logo)

To run the protocol generator using the demo json specification in the
package you can run:

``` r

library(ProtocolGenerator)

# to run the protocol generator with a demo json specification
test <- generateProtocol(
  jsonLocation = getDemoLoc(),
  webAPI = paste0('https://', 'api.ohdsi.org', '/WebAPI'),
  outputLocation = file.path(tempdir(), 'protocol'), 
  protocolSubheading = 'A demo protocol using the example specifcation json'
  #,headerColor = '4A1B12' # can optionally enter any HTML color code for the header
  #,headerLogoLocation = 'path to image' # can optionally add a logo to the header
)
```
