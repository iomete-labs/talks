<code style="white-space: pre-wrap; font-size: 10px; line-height: 1;">
module Map = Map.Make (Int)

type 'a orders = 'a Order.t list

type t =
  { asks : Order.ask orders Map.t
  ; bids : Order.bid orders Map.t }

let empty = { asks = Map.empty ; bids = Map.empty }

let orderbook asks bids =
  { asks = Map.of_seq asks
  ; bids = Map.of_seq bids }

let find_smallest_asks { asks ; _ } = Map.min_binding_opt asks

let find_largest_bids { bids ; _ } = Map.max_binding_opt bids

let insert_bid ({ bids ; _ } as t) price amount trader =
  let order = Order.new_bid amount trader in
  { t with
    bids = Map.update price (list_prepend_if_existing order) bids }

let insert_ask ({ asks ; _ } as t) price amount trader =
  let order = Order.new_ask amount trader in
  { t with
    asks = Map.update price (list_prepend_if_existing order) asks }

let update_smallest_asks_if_price_higher f price ({ asks ; _ } as t) =
  match find_smallest_asks t with
  | Some (price', _) when (price' > price) -> t, true
  | Some (key, _) -> { t with asks = Map.update key f asks }, false
  | None -> t, true

let update_largest_bids_if_price_lower f our_price ({ bids ; _ } as t) =
  match find_largest_bids t with
  | Some (s_price, _) when (s_price < our_price) -> t, true
  | Some (key, _) -> { t with bids = Map.update key f bids }, false
  | None -> t, true

let fill_once f t price amount =
  let remaining' = ref amount in
  let filled' = ref [] in
  let t, order_needed =
    f begin function
      | Some orders ->
        let filled, orders, remaining = Order.pop_until_filled amount orders in
        filled' := filled;
        remaining' := remaining;
        (* we should zero this map if the orders run out *)
        begin match orders with
          | [] -> None
          | _ -> Some orders
        end
      | None as n -> n
    end
      price
      t in
  order_needed,
  t,
  !remaining',
  !filled'

let fill f t price amount =
  let rec loop t acc remaining =
    let order_needed, t, remaining, filled = fill_once f t price remaining in
    match order_needed, remaining with
    | _, 0 -> false, t, remaining, acc
    | true, remaining -> true, t, remaining, (filled :: acc)
    | false, remaining -> loop t (filled :: acc) remaining in

  let order_needed, t, remaining, filled = loop t [] amount in
  order_needed, t, remaining, List.flatten filled

let fill_bids = fill update_largest_bids_if_price_lower

let fill_asks = fill update_smallest_asks_if_price_higher

let make_bid t price Order.{ amount ; trader } =
  let order_needed, t, remaining, filled = fill_asks t price amount in
  if order_needed then insert_bid t price remaining trader, filled
  else t, filled

let make_ask t price Order.{ amount ; trader } =
  let order_needed, t, remaining, filled = fill_bids t price amount in
  if order_needed then insert_ask t price remaining trader, filled
  else t, filled
</code>
