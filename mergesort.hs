merge [] [] = []
merge a [] = a
merge [] a = a
merge (a:as) (b:bs)
    | a < b = a:(merge as (b:bs))
    | otherwise = b:(merge (a:as) bs)

head_tail [] = error "what the fuck"
head_tail (x:xs) = (x,xs)
    
cut first l1 second l2
    | l1 == l2 = (first, second)
    | l2 - l1 == 1 = (first, second)
    | l2 > l1 = cut (first ++ [second_head]) (l1 + 1) second_tail (l2 -1)
    | l1 > l2 = error "what the fuck"
    where (second_head, second_tail) = head_tail second

merge_sort [] = []
merge_sort [x] = [x]
merge_sort arr = merge (merge_sort first_half) (merge_sort second_half)
                 where (first_half, second_half) = cut [] 0 arr (length arr)

main = do
    print (merge_sort [4,8,1,3,7,2,1,9,2,5,6])
