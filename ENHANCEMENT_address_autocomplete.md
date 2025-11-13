# Enhancement: Address Autocomplete

**Priority:** Low
**Status:** Proposed

## Description

Add address autocomplete/suggestions while typing in the Address Specificity tab to give users confidence that their address will be recognized.

## Technical Considerations

1. **Nominatim Autocomplete API**
   - Endpoint: `https://nominatim.openstreetmap.org/search?q=...&format=json&addressdetails=1&limit=5`
   - Requires proper rate limiting and caching
   - Must comply with Nominatim usage policy (max 1 request/sec)

2. **UI Implementation**
   - Use Shiny's `selectizeInput` with `server = TRUE` for dynamic options
   - Or implement custom JavaScript for autocomplete dropdown
   - Debounce input to avoid excessive API calls (e.g., wait 300ms after typing stops)

3. **Caching Strategy**
   - Cache recent autocomplete results to reduce API load
   - Clear cache periodically to keep memory usage reasonable

## Example Code Snippet

```r
# Potential implementation using selectizeInput with server-side options
selectizeInput(
  "addr",
  "Address (NYC)",
  choices = NULL,
  options = list(
    placeholder = "e.g., 1 Centre St, New York, NY",
    create = TRUE,
    persist = FALSE
  )
)

# In server
observeEvent(input$addr_search, {
  # Debounced query to Nominatim
  # Update selectizeInput choices
}, ignoreInit = TRUE)
```

## Alternatives

- Use a paid geocoding service with better autocomplete support (Google Places, Mapbox)
- Limit autocomplete to pre-loaded NYC landmarks/neighborhoods
- Skip autocomplete and rely on clear placeholder text and error messages

## Decision

Deferred to future iteration. Current implementation with clear placeholder and helpful error messages is sufficient for MVP.
