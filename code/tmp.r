tmp <- data.frame(votes=c(obj4$pansh, obj4$prish+obj4$pricsh, obj4$prdsh+obj4$prdcsh, obj4$ptsh, obj4$pvemsh, obj4$mcsh, obj4$panalsh, obj4$morenash, obj4$phsh, obj4$pssh, obj4$indep1sh, obj4$indep2sh),
                  seats=c(obj4$panw*obj4$ndis, (obj4$priw+obj4$pricw)*obj4$ndis, (obj4$prdw+obj4$prdcw)*obj4$ndis, obj4$ptw*obj4$ndis, obj4$pvemw*obj4$ndis, obj4$mcw*obj4$ndis, obj4$panalw*obj4$ndis, obj4$morenaw*obj4$ndis, obj4$phw*obj4$ndis, obj4$psw*obj4$ndis, obj4$indep1w*obj4$ndis, obj4$indep2w*obj4$ndis),
                  ndis=rep(obj4$ndis, times=12), N=rep(obj4$N, times=12), P=rep(12, times=(12*32)),
                  pty=c(rep(1, 32), rep(2, 32), rep(3, 32), rep(4, 32), rep(5, 32), rep(6, 32), rep(7, 32), rep(8, 32), rep(9, 32), rep(10, 32), rep(11, 32), rep(12, 32)),
                  lab=c(rep("pan", 32), rep("pric", 32), rep("prdc", 32), rep("pt", 32), rep("pvem", 32), rep("mc", 32), rep("panal", 32), rep("morena", 32), rep("ph", 32), rep("ps", 32), rep("indep", 32), rep("indep", 32)),
                  yr=rep(2012,times=(32*12)))
color <- c(rep("blue", 32), rep("red", 32), rep("gold", 32), rep("red", 32), rep("green", 32), rep("orange", 32), rep("cyan", 32), rep("brown", 32), rep("pink", 32), rep("purple", 32), rep("gray", 32), rep("gray", 32))
