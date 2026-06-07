-- Injects an SOP number + revision badge into HTML output only.
-- Reads sop-number and revision from the document YAML front matter.

function Meta(meta)
  -- only run for HTML output
  if not quarto.doc.is_format("html") then
    return meta
  end

  local sop_num = meta["sop-number"]
  local rev     = meta["revision"]

  -- skip pages that have no sop-number (e.g. index.qmd, orientation.qmd)
  if not sop_num then
    return meta
  end

  local num_str = pandoc.utils.stringify(sop_num)
  local rev_str = rev and pandoc.utils.stringify(rev) or "—"

  local badge = string.format([[
<div class="sop-meta-badge">
  <span class="sop-number">%s</span>
  <span class="sop-revision">Revision %s</span>
</div>
]], num_str, rev_str)

  -- prepend badge as a RawBlock before the document body
  return meta
end

function Pandoc(doc)
  if not quarto.doc.is_format("html") then
    return doc
  end

  local meta = doc.meta
  local sop_num = meta["sop-number"]
  if not sop_num then
    return doc
  end

  local num_str = pandoc.utils.stringify(sop_num)
  local rev     = meta["revision"]
  local rev_str = rev and pandoc.utils.stringify(rev) or "—"

  local badge = string.format([[
<div class="sop-meta-badge">
  <span class="sop-number">%s</span>
  <span class="sop-revision">Revision&nbsp;%s</span>
</div>
]], num_str, rev_str)

  local badge_block = pandoc.RawBlock("html", badge)

  -- Find the first non-Header block and insert the badge before it.
  -- This places the badge after the title (which Quarto renders from
  -- the YAML, not as a Header block) and before the first body content.
  local insert_pos = 1
  for i, block in ipairs(doc.blocks) do
    if block.t ~= "Header" then
      insert_pos = i
      break
    end
  end

  table.insert(doc.blocks, insert_pos, badge_block)
  return doc
end